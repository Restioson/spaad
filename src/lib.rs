#![feature(proc_macro_diagnostic)]
use proc_macro::TokenStream;
use syn::*;
use quote::{quote, format_ident};
use syn::spanned::Spanned;
use syn::parse_macro_input;
use syn::parse::{Parse, ParseStream};
use std::ops::Deref;

#[proc_macro_attribute]
pub fn entangled(_args: TokenStream, input: TokenStream) -> proc_macro::TokenStream {
    entangle(input)
}


enum EntangledItem {
    Struct(ItemStruct),
    Impl(ItemImpl),
}

impl Parse for EntangledItem {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        let expanded = if lookahead.peek(Token![pub]) ||
            lookahead.peek(Token![struct])
        {
            EntangledItem::Struct(input.parse()?)
        } else {
            EntangledItem::Impl(input.parse()?)
        };
        Ok(expanded)
    }
}

fn entangle(input: TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input as EntangledItem);
    let expanded = match item {
        EntangledItem::Struct(s) => entangle_struct(s),
        EntangledItem::Impl(i) => entangle_impl(i),
    };

    TokenStream::from(expanded)
}

fn entangle_struct(struct_def: ItemStruct) -> proc_macro2::TokenStream {
    let name = struct_def.ident;
    let actor = format_ident!("{}Actor", name);
    let vis = struct_def.vis;
    let fields = struct_def.fields;

    let (impl_generics, ty_generics, where_clause) = struct_def.generics.split_for_impl();

    quote! {
        #vis struct #name#impl_generics #where_clause {
            addr: xtra::Address<#actor#ty_generics>,
        }

        struct #actor#impl_generics #where_clause #fields
    }
}

fn entangle_impl(impl_block: ItemImpl) -> proc_macro2::TokenStream {
    match &impl_block.trait_ {
        Some(trait_) if trait_.1.segments.last().unwrap().ident == "Actor" => {
            entangle_actor_impl(impl_block)
        },
        None => entangle_handlers_impl(impl_block),
        _ => {
            impl_block.span()
                .unwrap()
                .error("`spaad::entangled` can only be called impls of `xtra::Actor`")
                .emit();
            unreachable!()
        },
    }
}

fn get_name_from_ty(ty: &syn::Type) -> &proc_macro2::Ident {
    let ty_path = match &*ty {
        Type::Path(path) => &path.path,
        _ => {
            ty.span()
                .unwrap()
                .error(
                    "the return type of a `spaad::entangled` handler must be\
                    `Result<T, xtra::Disconnected>`"
                )
                .emit();
            unreachable!()
        },
    };
    &ty_path.segments.last().unwrap().ident
}

fn get_name(block: &ItemImpl) -> &proc_macro2::Ident {
    let self_ty_path = match &*block.self_ty {
        Type::Path(path) => &path.path,
        _ => {
            block.self_ty.span()
                .unwrap()
                .error("the self type of a `spaad::entangled` impl must be a struct")
                .emit();
            unreachable!()
        },
    };
    &self_ty_path.segments.last().unwrap().ident
}

fn entangle_handlers_impl(handlers_impl: ItemImpl) -> proc_macro2::TokenStream {
    let (impl_generics, ty_generics, where_clause) = handlers_impl.generics.split_for_impl();
    let actor_items = handlers_impl.items.iter();
    let transformed_items = transform_items(&handlers_impl, handlers_impl.items.iter());
    let name = get_name(&handlers_impl);
    let actor = format_ident!("{}Actor", name);

    quote! {
        impl#impl_generics #actor#ty_generics #where_clause {
            #(#actor_items)*
        }

        impl#impl_generics #name#ty_generics #where_clause {
            #(#transformed_items)*
        }
    }
}

fn transform_items<'a, I: Iterator<Item = &'a ImplItem> + 'a>(
    impl_block: &'a ItemImpl,
    iter: I
) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
    iter
        .map(move |method| match method {
            ImplItem::Const(c) => quote!(#c),
            ImplItem::Type(t) => quote!(#t),
            ImplItem::Macro(m) => quote!(#m),
            ImplItem::Verbatim(v) => quote!(#v),
            ImplItem::Method(m) => transform_method(impl_block,m.clone()),
            _ => unimplemented!("Unknown impl item"),
        })
}

// I know, I apologize.
fn transform_method(
    impl_block: &ItemImpl,
    method: ImplItemMethod,
) -> proc_macro2::TokenStream {
    let name = get_name(&impl_block);
    let actor_name = format_ident!("{}Actor", name);
    let (impl_generics, ty_generics, _) = impl_block.generics.split_for_impl();

    let ImplItemMethod { attrs, vis, mut sig, .. } = method;

    if sig.asyncness.is_none() &&
        matches!(sig.inputs.first(), Some(FnArg::Typed(_)) | None)
    {
        let fn_name = &sig.ident;
        let inputs: Vec<_> = sig.inputs
            .iter()
            .map(|arg| match arg {
                FnArg::Typed(t) => Some(&t.pat),
                _ => unreachable!(),
            })
            .collect();
        let arg_inputs = &sig.inputs;

        if fn_name.to_string() == "new" {
            if let ReturnType::Type(_, ty) = &sig.output {
                let ret_name = get_name_from_ty(&ty).to_string();
                if ret_name == "Self" || ret_name == name.to_string() {
                    let turbo = ty_generics.as_turbofish();
                    #[cfg(any(feature = "with-tokio-0_2", feature = "with-async_std-1"))]
                    let spawn = quote! {
                        #[allow(dead_code)]
                        #(#attrs)* #vis fn spawn(#arg_inputs) -> Self {
                            use xtra::prelude::*;
                            let act = #actor_name#turbo::new(#(#inputs),*);
                            let addr = act.spawn();
                            #name { addr }
                        }
                    };
                    let create = quote! {
                        #[allow(dead_code)]
                        #(#attrs)* #vis fn create(
                            #arg_inputs
                        ) -> (Self, xtra::ActorManager<#actor_name#ty_generics>) {
                            use xtra::prelude::*;
                            let act = #actor_name#turbo::new(#(#inputs),*);
                            let (addr, mgr) = act.create();
                            (#name { addr }, mgr)
                        }
                    };
                    #[cfg(any(feature = "with-tokio-0_2", feature = "with-async_std-1"))]
                    return quote!(#spawn #create);
                    #[allow(unreachable_code)]
                    return quote!(#create)
                }
            }

            sig.output.span()
                .unwrap()
                .error("functions named new in `spaad::entangled` impl blocks must return `Self`")
                .emit();
            unreachable!()
        }

        return quote! {
            #(#attrs)* #vis #sig {
                #actor_name::#ty_generics::#fn_name(#(#inputs),*)
            }
        }
    }

    match sig.inputs.first_mut() {
        Some(FnArg::Typed(_)) | None => {
            sig.span()
                .unwrap()
                .error("handlers in `spaad::entangled` impl blocks must take `self`")
                .emit();
        },
        Some(FnArg::Receiver(recv)) => {
            recv.mutability = None;
        }
    }

    let call_inputs = sig.inputs.iter()
        .cloned()
        .skip(1)
        .map(|arg| {
            let span = arg.span();
            match arg {
                FnArg::Typed(mut t) => {
                    match &mut *t.pat {
                        Pat::Type(_) => {},
                        Pat::Ident(ref mut pat) => pat.mutability = None,
                        _ => {
                            span
                                .unwrap()
                                .error(
                                    "`spaad::entangle` only supports type patterns \
                                         (e.g `f: f64`) and ident patterns (e.g `mut f: f64`)")
                                .emit();
                            unreachable!()
                        }
                    }

                    t
                },
                _ => unreachable!(),
            }
        })
        .filter(|PatType { ty, .. }| match &**ty {
            Type::Reference(ty_ref) => {
                if get_name_from_ty(&ty_ref.elem).to_string() != "Context" {
                    ty_ref.span()
                        .unwrap()
                        .warning("cannot take by reference in a `spaad::entangle` handler")
                        .emit();
                } else {
                    return false;
                }
                true
            }
            _ => true,
        });
    let msg_members = call_inputs
        .clone();
    let msg_members_destructured: Vec<_> = msg_members.clone()
        .map(|PatType { pat, .. }| pat)
        .collect();
    let output = &sig.output;
    let (handle_result, output, result) = match transform_ret(&sig.output) {
        Some(output) => {
            (
                quote! { .and_then(|x| x) },
                quote!(#output),
                output,
            )
        },
        None if matches!(output, ReturnType::Default) => {
            (
                quote! { .expect("actor disconnected") },
                quote!(()),
                quote!(()),
            )
        },
        None => {
            (
                quote! { .expect("actor disconnected") },
                quote!(()),
                quote!(#output),
            )
        },
    };
    let fn_name = &sig.ident;
    let mut ctx_idx: Option<usize> = None;
    let fn_decl_inputs: Vec<FnArg> = sig.inputs.iter()
        .cloned()
        .enumerate()
        .filter(|(i, arg)| match arg {
            FnArg::Typed(PatType { ty, .. }) => {
                if let Type::Reference(ty_ref) = &**ty {
                    if get_name_from_ty(&ty_ref.elem).to_string() != "Context" {
                        ty_ref.span()
                            .unwrap()
                            .warning("cannot take by reference in a `spaad::entangle` handler")
                            .emit();
                    } else {
                        ctx_idx = Some(*i);
                        return false;
                    }
                }

                true
            },
            _ => true,
        })
        .map(|(_, x)| x)
        .collect();

    let mut call_inputs: Vec<proc_macro2::TokenStream> = call_inputs
        .map(|PatType { pat, .. }| {
            if let Pat::Ident(id) = &*pat {
                let id = &id.ident;
                return quote!(#id)
            }
            unreachable!("{:?}", pat)
        }) //
        .collect();
    if let Some(ctx_idx) = ctx_idx {
        // -1 because 0 is self
        call_inputs.insert(ctx_idx - 1,quote!(ctx))
    }

    quote! {
        #[allow(unused_mut)]
        #(#attrs)* #vis fn #fn_name(
            #(#fn_decl_inputs),*
        ) -> impl std::future::Future<Output = #output>  {
            use xtra::prelude::*;

            struct Msg { #(#msg_members)* };

            impl xtra::Message for Msg {
                type Result = #result;
            }

            impl#impl_generics Handler<Msg> for #actor_name#ty_generics {
                type Responder<'a> = impl std::future::Future<Output = #result> + 'a;

                fn handle<'a>(&'a mut self, m: Msg, ctx: &'a mut Context<Self>) -> Self::Responder<'a> {
                    let Msg { #(#msg_members_destructured)* } = m;
                    self.#fn_name(#(#call_inputs),*)
                }
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
                            return Some(quote!(#ret_ty))
                        }
                    }
                }
            }
        }
    }

    None
}

fn entangle_actor_impl(actor_impl: ItemImpl) -> proc_macro2::TokenStream {
    let (impl_generics, ty_generics, where_clause) = actor_impl.generics.split_for_impl();
    let items = actor_impl.items.iter();
    let name = format_ident!("{}Actor", get_name(&actor_impl));

    quote! {
        impl#impl_generics xtra::Actor for #name#ty_generics #where_clause {
            #(#items)*
        }
    }
}
