#![cfg_attr(doc, allow(unused_braces))]
#![feature(proc_macro_diagnostic)]
use proc_macro::TokenStream;
use syn::*;
use quote::{quote, format_ident};
use syn::spanned::Spanned;
use syn::parse_macro_input;
use syn::parse::{Parse, ParseStream};
use std::ops::Deref;

/// The main item of the crate. This is a proc macro used as an attribute on the actor struct
/// definition, `Actor` implementation, and on an impl block in which the handler functions are used.
///
/// For example:
///
/// ```rust,ignore
/// #![feature(type_alias_impl_trait, generic_associated_types)]
///
/// #[spaad::entangled]
/// struct Printer {
///     times: usize,
/// }
///
/// #[spaad::entangled]
/// impl Actor for Printer {
///     fn started(&mut self, _ctx: &mut Context<Self>) {
///         println!("Actor started!");
///     }
/// }
///
/// #[spaad::entangled]
/// impl Printer {
///     // New is a special case: if it is present, create() and spawn() functions are emitted too
///     fn new() -> Self {
///         // You must use Self when referring to the type itself, as it is internally renamed
///         // something else. This allows for better IDE support.
///         Self { times: 0 }
///     }
///
///     async fn print(&mut self, string: String) {
///         self.times += 1;
///         println!("Printing {}. Printed {} times so far.", string, self.times);
///     }
/// }
/// ```
///
/// It is important to note that the `new` function is a special case. If it is present, the proc
/// macro will also emit a `create` method for the actor wrapper. It can take arguments. If the
/// `with-tokio-0_2` or `with-async_std-1_0` features are enabled, then it will also emit a `spawn`
/// method. These are both very similar to the `xtra::Actor` methods of the same names.
///
/// Messages can then be sent to actors as such:
/// ```rust,ignore
/// my_actor.print().await;
/// ```
///
/// The output type of the future will be determined by the signature. It will be identical to the
/// written type, except when the return is written as `Result<T, xtra::Disconnected>` (see below).
///
/// If you do not want to `await` for the message to complete processing, you can do the following:
/// ```rust,ignore
/// let _ = my_actor.print(); // Binding to avoid #[must_use] warning on Future
/// ```
///
/// This will also mean that the return type will be discarded, as the receiving end of the channel
/// will be dropped.
///
/// The proc macro will create the given struct (in the example's case, `Printer`). This is just a
/// wrapper for an actor  address to the actual structure, which is strictly internal and cannot be
/// interacted with except by sending messages. This is why `Self` must be used, and not the
/// actual name of the actor struct. Currently, it is renamed to `__{Actor}`, but this is not stable
/// and could change in the future.
///
/// The methods to send messages will panic if the actor is disconnected. If you want to manually
/// handle this error, make the return type of the handler function `Result<T, xtra::Disconnected>`.
/// The type must be named `Context` - it cannot be renamed by re-importing. If you want to access
/// the actor cotnext add an argument to the function with `&mut Context<Self>` as the type.
/// Similarly, the type must be named `Context` - it cannot be renamed by re-importing.
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
        let attrs = input.call(Attribute::parse_outer)?;
        let lookahead = input.lookahead1();
        let expanded = if lookahead.peek(Token![impl]) {
            let mut item: ItemImpl = input.parse()?;
            item.attrs = attrs;
            EntangledItem::Impl(item)
        } else {
            let mut item: ItemStruct = input.parse()?;
            item.attrs = attrs;
            EntangledItem::Struct(item)
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
    let ItemStruct { attrs, vis, ident, generics, fields, semi_token, .. } = struct_def;
    let actor = format_ident!("__{}Actor", ident);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        #[derive(Clone)]
        #vis struct #ident#impl_generics #where_clause {
            addr: ::spaad::export::xtra::Address<#actor#ty_generics>,
        }

        impl#impl_generics #ident#ty_generics #where_clause {
            #vis fn into_address(self) -> ::spaad::export::xtra::Address<#actor#ty_generics> {
                self.addr
            }

            #vis fn address(&self) -> &::spaad::export::xtra::Address<#actor#ty_generics> {
                &self.addr
            }
        }

        #(#attrs)*
        #[doc(hidden)]
        pub struct #actor#impl_generics #where_clause #fields #semi_token
    }
}

fn entangle_impl(impl_block: ItemImpl) -> proc_macro2::TokenStream {
    match &impl_block.trait_ {
        Some(_) => entangle_trait_impl(impl_block),
        None => entangle_handlers_impl(impl_block),
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
    let actor = format_ident!("__{}Actor", name);

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
    let actor_name = format_ident!("__{}Actor", name);
    let (impl_generics, ty_generics, where_clause) = impl_block.generics.split_for_impl();

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
                            use ::spaad::export::xtra::prelude::*;
                            let act = #actor_name#turbo::new(#(#inputs),*);
                            let addr = act.spawn();
                            #name { addr }
                        }
                    };
                    let create = quote! {
                        #[allow(dead_code)]
                        #(#attrs)* #vis fn create(
                            #arg_inputs
                        ) -> (Self, ::spaad::export::xtra::ActorManager<#actor_name#ty_generics>) {
                            use ::spaad::export::xtra::prelude::*;
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

    #[cfg(feature = "stable")]
    let handle = quote! {
        async fn handle(&mut self, m: Msg, ctx: &mut Context<Self>) -> #result {
            let Msg { #(#msg_members_destructured)* } = m;
            self.#fn_name(#(#call_inputs),*).await
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
    let responder = quote !{
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
                            return Some(quote!(#ret_ty))
                        }
                    }
                }
            }
        }
    }

    None
}

fn entangle_trait_impl(mut trait_impl: ItemImpl) -> proc_macro2::TokenStream {
    match &mut *trait_impl.self_ty {
        Type::Path(ref mut path) => {
            let last_seg = path.path.segments.last_mut().unwrap();
            let ident = &mut last_seg.ident;
            *ident = format_ident!("__{}Actor", ident);
        }
        _ => {
            trait_impl.span()
                .unwrap()
                .error("`spaad::entangle` can only be called on impls of an actor struct")
                .emit()
        }
    }

    quote!(#trait_impl)
}
