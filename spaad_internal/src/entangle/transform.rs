use crate::entangle::{get_actor_name, get_name, ty_is_name};
use proc_macro_error::abort;
use quote::{format_ident, quote};
use std::ops::Deref;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;
use proc_macro2::TokenStream;

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

fn is_context_ref(ty_ref: &TypeReference, is_handler: bool) -> bool {
    let is_static = ty_ref
        .lifetime
        .as_ref()
        .map(|lif| lif.ident == "static")
        .unwrap_or(false);
    let is_context = ty_is_name(&ty_ref.elem, "Context");
    if !is_context && !is_static && is_handler {
        abort!(ty_ref, "must be 'static or a reference to `Context<Self>`")
    }

    is_context
}

// I know, I apologize.
pub fn transform_method(impl_block: &ItemImpl, method: ImplItemMethod) -> proc_macro2::TokenStream {
    let name = get_name(&impl_block);
    let actor_name = get_actor_name(&impl_block);
    let act_ty_generics = impl_block.generics.split_for_impl().1;

    let has_attr = |name| find_attr(name, &method.attrs).is_some();
    let is_handler = has_attr("handler");

    if !has_attr("handler") {
        // matches checks for no receiver, i.e a static method
        if matches!(method.sig.inputs.first(), Some(FnArg::Typed(_)) | None) {
            return transform_static_methods(
                name,
                actor_name,
                has_attr("create"),
                has_attr("spawn"),
                method,
                act_ty_generics,
            );
        } else {
            return quote!();
        }
    }

    let ImplItemMethod {
        attrs,
        vis,
        mut sig,
        ..
    } = method;

    match sig.inputs.first_mut() {
        Some(FnArg::Typed(_)) | None => {
            abort!(
                sig,
                "handlers in `spaad::entangled` impl blocks must take `self`"
            );
        }
        Some(FnArg::Receiver(recv)) => {
            recv.mutability = None;
        }
    }

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
                    abort!(
                        span,
                        "`spaad::entangle` only support simple patterns (e.g `mut f: f64`)"
                    )
                }

                return pat_type;
            }
            unreachable!()
        })
        .filter(|PatType { ty, .. }| {
            if let Type::Reference(ty_ref) = &**ty {
                return !is_context_ref(ty_ref, is_handler);
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
        None => {
            let output = match output {
                ReturnType::Type(_, ty) => ty,
                _ => unreachable!(),
            };

            (
                quote! { .expect("actor disconnected") },
                quote!(()),
                quote!(#output),
            )
        },
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
                    if is_context_ref(ty_ref, is_handler) {
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

    let (fn_impl_generics, fn_ty_generics, fn_where) = sig.generics.split_for_impl();
    let fn_turbo = fn_ty_generics.as_turbofish();

    let mut handler_generics: Generics = impl_block.generics.clone();
    for generic in sig.generics.params.clone() {
        handler_generics.params.push(generic);
    }

    let (handler_impl_generics, _, handler_where) = handler_generics.split_for_impl();

    let handler = if sig.asyncness.is_some() {
        #[cfg(feature = "stable")]
        let handle = quote! {
            async fn handle(
                &mut self,
                m: Msg#fn_ty_generics,
                ctx: &mut ::spaad::export::xtra::Context<Self>,
            ) -> #result {
                let Msg { #(#msg_members_destructured)* } = m;
                self.#fn_name#fn_turbo(#(#call_inputs),*).await
            }
        };
        #[cfg(not(feature = "stable"))]
        let handle = quote! {
            fn handle<'a>(
                &'a mut self,
                m: Msg#fn_ty_generics,
                ctx: &'a mut ::spaad::export::xtra::Context<Self>,
            ) -> Self::Responder<'a> {
                let Msg { #(#msg_members_destructured)* } = m;
                self.#fn_name#fn_turbo(#(#call_inputs),*)
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
            #async_trait
            #[allow(unused_variables)]
            impl#handler_impl_generics
                ::spaad::export::xtra::Handler<Msg#fn_ty_generics>
            for #actor_name#act_ty_generics
                 #handler_where
            {
                #responder #handle
            }
        }
    } else {
        quote! {
            #[allow(unused_variables)]
            impl#handler_impl_generics
                ::spaad::export::xtra::SyncHandler<Msg#fn_ty_generics>
            for #actor_name#act_ty_generics
                 #handler_where
            {
                fn handle(
                    &mut self,
                    m: Msg#fn_ty_generics,
                    ctx: &mut ::spaad::export::xtra::Context<Self>
                ) -> #result {
                    let Msg { #(#msg_members_destructured)* } = m;
                    self.#fn_name#fn_turbo(#(#call_inputs),*)
                }
            }
        }
    };

    quote! {
        #[allow(unused_mut)]
        #(#attrs)* #vis fn #fn_name#fn_impl_generics(
            #(#fn_decl_inputs),*
        ) -> impl std::future::Future<Output = #output>
            #fn_where
        {
            use ::spaad::export::xtra::prelude::*;

            struct Msg#fn_impl_generics #fn_where { #(#msg_members)* };

            impl#fn_impl_generics ::spaad::export::xtra::Message for Msg#fn_ty_generics
                #fn_where
            {
                type Result = #result;
            }

            #handler

            let f = self.addr.send(Msg#fn_turbo { #(#msg_members_destructured)* });
            async { f.await#handle_result }
        }
    }
}

fn transform_ret(r: &ReturnType) -> Option<proc_macro2::TokenStream> {
    if let ReturnType::Type(_, ret_ty) = r {
        if ty_is_name(ret_ty, "Result") {
            if let Type::Path(ty_path) = ret_ty.deref() {
                let arg = &ty_path.path.segments.last().unwrap().arguments;
                if let PathArguments::AngleBracketed(generics) = arg {
                    let last = generics.args.last().unwrap();
                    if let GenericArgument::Type(ty) = last {
                        if ty_is_name(ty, "Disconnected") {
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
    has_create: bool,
    has_spawn: bool,
    method: ImplItemMethod,
    impl_ty_generics: TypeGenerics,
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
        transform_constructors(
            name,
            actor_name,
            method.clone(),
            impl_ty_generics,
            arg_inputs,
            inputs,
        )
    } else {
        let fn_name = &sig.ident;
        let method_ty_generics = sig.generics.split_for_impl().1;
        let method_turbo = method_ty_generics.as_turbofish();
        let act_turbo = impl_ty_generics.as_turbofish();
        let ImplItemMethod { attrs, vis, .. } = method;

        quote! {
            #(#attrs)* #vis #sig {
                #actor_name#act_turbo::#fn_name#method_turbo(#(#inputs),*)
            }
        }
    }
}

fn transform_constructors<'a>(
    name: &Ident,
    actor_name: proc_macro2::TokenStream,
    method: ImplItemMethod,
    act_ty_generics: TypeGenerics,
    arg_inputs: &Punctuated<FnArg, Token![,]>,
    inputs: Vec<&Box<Pat>>,
) -> proc_macro2::TokenStream {
    let ImplItemMethod {
        attrs, vis, sig, ..
    } = method;
    let is_name = |ty, name| ty_is_name(ty, name);

    if matches!(
        &sig.output,
        ReturnType::Type(_, ty) if !(is_name(ty, &name.to_string()) || is_name(ty, "Self"))
    ) {
        abort!(
            sig.output,
            "functions annotated with `spawn` or `create` must return `Self`"
        );
    }

    let (impl_generics, ty_generics, where_clause) = sig.generics.split_for_impl();
    let act_turbo = act_ty_generics.as_turbofish();
    let fn_turbo = ty_generics.as_turbofish();
    let act_fn_name = &sig.ident;

    #[allow(unused_mut)]
    let mut spawn: Option<TokenStream> = None;
    #[allow(unused_variables)]
    if let Some(attr) = get_attr("spawn", &attrs) {
        #[cfg(any(feature = "with-tokio-0_2", feature = "with-async_std-1"))]
        {
            let fn_name = get_ctor_name(&sig, attr);
            spawn = Some(quote! {
                #(#attrs)* #vis fn #fn_name#impl_generics(#arg_inputs) -> Self #where_clause {
                    use ::spaad::export::xtra::prelude::*;
                    let act = #actor_name#act_turbo::#act_fn_name#fn_turbo(#(#inputs),*);
                    let addr = act.spawn();
                    #name { addr }
                }
            });
        }
    };

    let mut create = None;
    if let Some(attr) = get_attr("create", &attrs) {
        let fn_name = get_ctor_name(&sig, attr);
        create = Some(quote! {
            #(#attrs)* #vis fn #fn_name#impl_generics(
                #arg_inputs
            ) -> (Self, ::spaad::export::xtra::ActorManager<#actor_name#act_ty_generics>)
                #where_clause
            {
                use ::spaad::export::xtra::prelude::*;
                let act = #actor_name#act_turbo::#act_fn_name#fn_turbo(#(#inputs),*);
                let (addr, mgr) = act.create();
                (#name { addr }, mgr)
            }
        })
    };

    return quote!(#spawn #create);
}

fn get_ctor_name(sig: &Signature, attr: &Attribute) -> Ident {
    let fn_name = sig.ident.clone();
    // thx to serde for this fn
    match attr.parse_meta() {
        Ok(Meta::List(MetaList { nested, .. })) => match nested.first().unwrap() {
            NestedMeta::Meta(Meta::NameValue(name)) if name.path.is_ident("rename") => {
                match &name.lit {
                    Lit::Str(lit) => format_ident!("{}", lit.value()),
                    _ => abort!(attr, "Expected rename target to be a string"),
                }
            }
            _ => abort!(
                attr,
                "Only one valid argument here: `rename = \"{target}\"`"
            ),
        },
        _ => fn_name,
    }
}
