use crate::entangle::{get_actor_name, get_name, get_name_from_ty, set_visibility_min_pub_super};
use quote::{format_ident, quote};
use std::ops::Deref;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

fn is_attr(name: &str, attr: &Attribute) -> bool {
    let mut iter = attr.path.segments.iter();
    iter.next_back().map(|i| i.ident.to_string()).as_deref() == Some(name)
}

fn find_attr(name: &str, attrs: &[Attribute]) -> Option<usize> {
    attrs.iter().position(|attr| is_attr(name, attr))
}

fn get_attr<'a>(name: &str, attrs: &'a [Attribute]) -> Option<&'a Attribute> {
    attrs.iter().find(|attr| is_attr(name, attr))
}

// I know, I apologize.
pub fn transform_method(impl_block: &ItemImpl, mut method: ImplItemMethod) -> proc_macro2::TokenStream {
    let name = get_name(&impl_block);
    let actor_name = get_actor_name(&impl_block);

    let has_attr = |name| find_attr(name, &method.attrs).is_some();
    let (has_create, has_spawn) = (has_attr("create"), has_attr("spawn"));
    let should_transform = has_attr("handler") | has_create | has_spawn;

    set_visibility_min_pub_super(&mut method.vis);

    let ImplItemMethod {
        attrs,
        vis,
        mut sig,
        ..
    } = method.clone();

    if !should_transform {
        return quote!();
    }

    // matches checks for no receiver, i.e a static method
    if matches!(sig.inputs.first(), Some(FnArg::Typed(_)) | None) {
        return transform_static_methods(name, actor_name, method, has_create, has_spawn);
    }

    match sig.inputs.first_mut() {
        Some(FnArg::Typed(_)) | None => {
            sig.span()
                .unwrap()
                .error("handlers in `spaad::entangled` impl blocks must take `self`")
                .emit();
        }
        Some(FnArg::Receiver(recv)) => {
            recv.mutability = None;
        }
    }

    let (impl_generics, ty_generics, where_clause) = impl_block.generics.split_for_impl();

    let call_inputs = sig
        .inputs
        .iter()
        .cloned()
        .skip(1)
        .map(|arg| {
            let span = arg.span();
            if let FnArg::Typed(mut pat_type) = arg {
                if let Pat::Ident(ref mut pat) = &mut *pat_type.pat {
                    pat.mutability = None;
                }

                if !matches!(*pat_type.pat, Pat::Type(_) | Pat::Ident(_)) {
                    span.unwrap()
                        .error("`spaad::entangle` only support simple patterns (e.g `mut f: f64`)")
                        .emit();
                }

                return pat_type;
            }
            unreachable!()
        })
        .filter(|PatType { ty, .. }| {
            if let Type::Reference(ty_ref) = &**ty {
                if get_name_from_ty(&ty_ref.elem).to_string() != "Context" {
                    ty_ref
                        .span()
                        .unwrap()
                        .warning("cannot take by reference in a `spaad::entangle` handler")
                        .emit();
                } else {
                    return false;
                }
            }

            true
        });

    let msg_members = call_inputs.clone();
    let msg_members_destructured: Vec<_> =
        msg_members.clone().map(|PatType { pat, .. }| pat).collect();
    let output = &sig.output;
    let (handle_result, output, result) = match transform_ret(&sig.output) {
        Some(output) => (quote! { .and_then(|x| x) }, quote!(#output), output),
        None if matches!(output, ReturnType::Default) => (
            quote! { .expect("actor disconnected") },
            quote!(()),
            quote!(()),
        ),
        None => (
            quote! { .expect("actor disconnected") },
            quote!(()),
            quote!(#output),
        ),
    };
    let fn_name = &sig.ident;
    let mut ctx_idx: Option<usize> = None;
    let fn_decl_inputs: Vec<FnArg> = sig
        .inputs
        .iter()
        .cloned()
        .enumerate()
        .filter(|(i, arg)| match arg {
            FnArg::Typed(PatType { ty, .. }) => {
                if let Type::Reference(ty_ref) = &**ty {
                    if get_name_from_ty(&ty_ref.elem).to_string() != "Context" {
                        ty_ref
                            .span()
                            .unwrap()
                            .warning("cannot take by reference in a `spaad::entangle` handler")
                            .emit();
                    } else {
                        ctx_idx = Some(*i);
                        return false;
                    }
                }

                true
            }
            _ => true,
        })
        .map(|(_, x)| x)
        .collect();

    let mut call_inputs: Vec<proc_macro2::TokenStream> = call_inputs
        .map(|PatType { pat, .. }| {
            if let Pat::Ident(id) = &*pat {
                let id = &id.ident;
                return quote!(#id);
            }
            unreachable!("{:?}", pat)
        }) //
        .collect();

    if let Some(ctx_idx) = ctx_idx {
        // -1 because 0 is self
        call_inputs.insert(ctx_idx - 1, quote!(ctx))
    }

    #[cfg(feature = "stable")]
    let dot_await = if sig.asyncness.is_some() {
        quote!(.await)
    } else {
        quote!()
    };

    #[cfg(feature = "stable")]
    let handle = quote! {
        async fn handle(&mut self, m: Msg, ctx: &mut Context<Self>) -> #result {
            let Msg { #(#msg_members_destructured)* } = m;
            self.#fn_name(#(#call_inputs),*)#dot_await
        }
    };
    #[cfg(not(feature = "stable"))]
    let handle = quote! {
        fn handle<'a>(&'a mut self, m: Msg, ctx: &'a mut Context<Self>) -> Self::Responder<'a> {
            let Msg { #(#msg_members_destructured)* } = m;
            self.#fn_name(#(#call_inputs),*)
        }
    };

    #[cfg(not(feature = "stable"))]
    let responder = quote! {
        type Responder<'a> = impl std::future::Future<Output = #result> + 'a;
    };
    #[cfg(feature = "stable")]
    let responder = quote!();

    #[cfg(not(feature = "stable"))]
    let async_trait = quote!();
    #[cfg(feature = "stable")]
    let async_trait = quote!(#[::spaad::export::async_trait::async_trait]);

    quote! {
        #[allow(unused_mut)]
        #(#attrs)* #vis fn #fn_name(
            #(#fn_decl_inputs),*
        ) -> impl std::future::Future<Output = #output>  {
            use ::spaad::export::xtra::prelude::*;

            struct Msg { #(#msg_members)* };

            impl ::spaad::export::xtra::Message for Msg {
                type Result = #result;
            }

            #async_trait
            #[allow(unused_variables)]
            impl#impl_generics Handler<Msg> for #actor_name#ty_generics #where_clause {
                #responder #handle
            }

            let f = self.addr.send(Msg{ #(#msg_members_destructured)* });
            async { f.await#handle_result }
        }
    }
}

fn transform_ret(r: &ReturnType) -> Option<proc_macro2::TokenStream> {
    if let ReturnType::Type(_, ret_ty) = r {
        let name = get_name_from_ty(ret_ty);

        if name == "Result" {
            if let Type::Path(ty_path) = ret_ty.deref() {
                let arg = &ty_path.path.segments.last().unwrap().arguments;
                if let PathArguments::AngleBracketed(generics) = arg {
                    let last = generics.args.last().unwrap();
                    if let GenericArgument::Type(ty) = last {
                        if get_name_from_ty(ty) == "Disconnected" {
                            return Some(quote!(#ret_ty));
                        }
                    }
                }
            }
        }
    }

    None
}

fn transform_static_methods(
    name: &Ident,
    actor_name: proc_macro2::TokenStream,
    method: ImplItemMethod,
    has_create: bool,
    has_spawn: bool,
) -> proc_macro2::TokenStream {
    let sig = &method.sig;
    let arg_inputs = &sig.inputs;
    let inputs: Vec<_> = sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Typed(t) => Some(&t.pat),
            _ => None,
        })
        .collect();

    if has_create || has_spawn {
        transform_constructors(name, actor_name, method.clone(), arg_inputs, inputs)
    } else {
        let fn_name = &sig.ident;
        let ImplItemMethod { attrs, vis, .. } = method;
        let ty_generics = sig.generics.split_for_impl().1;
        quote! {
            #(#attrs)* #vis #sig {
                #actor_name::#ty_generics::#fn_name(#(#inputs),*)
            }
        }
    }
}

fn transform_constructors<'a>(
    name: &Ident,
    actor_name: proc_macro2::TokenStream,
    method: ImplItemMethod,
    arg_inputs: &Punctuated<FnArg, Token![,]>,
    inputs: Vec<&Box<Pat>>,
) -> proc_macro2::TokenStream {
    let ImplItemMethod {
        attrs, vis, sig, ..
    } = method.clone();
    let get = |ty| get_name_from_ty(ty);

    if matches!(
        &sig.output,
        ReturnType::Type(_, ty) if !(get(&ty) == name || (*get(&ty) == format_ident!("Self"))))
    {
        sig.output
            .span()
            .unwrap()
            .error("functions annotated with `spawn` or `create` must return `Self`")
            .emit();
    }

    let ty_generics = method.sig.generics.split_for_impl().1;
    let turbo = ty_generics.as_turbofish();

    let mut spawn = None;
    if let Some(attr) = get_attr("spawn", &attrs) {
        #[cfg(any(feature = "with-tokio-0_2", feature = "with-async_std-1"))]
        {
            let fn_name = get_ctor_name(&method.sig, attr);
            spawn = Some(quote! {
                #(#attrs)* #vis fn #fn_name(#arg_inputs) -> Self {
                    use ::spaad::export::xtra::prelude::*;
                    let act = #actor_name#turbo::new(#(#inputs),*);
                    let addr = act.spawn();
                    super::#name { addr }
                }
            });
        }
    };

    let mut create = None;
    if let Some(attr) = get_attr("create", &attrs) {
        let fn_name = get_ctor_name(&method.sig, attr);
        create = Some(quote! {
            #(#attrs)* #vis fn #fn_name(
                #arg_inputs
            ) -> (Self, ::spaad::export::xtra::ActorManager<#actor_name#ty_generics>) {
                use ::spaad::export::xtra::prelude::*;
                let act = #actor_name#turbo::new(#(#inputs),*);
                let (addr, mgr) = act.create();
                (super::#name { addr }, mgr)
            }
        })
    };

    return quote!(#spawn #create);
}

fn get_ctor_name(sig: &Signature, attr: &Attribute) -> Ident {
    let fn_name = sig.ident.clone();
    // thx to serde for this fn
    match attr.parse_meta() {
        Ok(Meta::NameValue(m)) if m.path.is_ident("rename") => match m.lit {
            Lit::Str(lit) => format_ident!("{}", lit.value()),
            _ => {
                attr.span()
                    .unwrap()
                    .error("Expected rename target to be a string")
                    .emit();
                unreachable!()
            }
        },
        _ => fn_name,
    }
}
