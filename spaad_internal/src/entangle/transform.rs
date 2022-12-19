use crate::entangle::{get_actor_name, get_name, ty_is_name};
use proc_macro2::TokenStream;
use proc_macro_error::abort;
use quote::{format_ident, quote};
use std::collections::HashMap;
use std::ops::Deref;
use proc_macro2::TokenStream as TokenStream2;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::*;

type AttrMap = HashMap<String, Option<HashMap<String, (Ident, Lit)>>>;

fn parse_attributes(attrs: &[Attribute]) -> AttrMap {
    let mut attributes = HashMap::new();

    for attr in attrs.iter() {
        let meta = match attr.parse_meta() {
            Ok(meta) => meta,
            Err(error) => abort!(attr, "Failed parsing as Meta: {}", error),
        };

        let (path, nested) = match meta {
            // #[spaad::thing]
            Meta::Path(path) => (path, None),
            // #[spaad::thing(with = args)]
            Meta::List(list) => (list.path, Some(list.nested)),
            Meta::NameValue(_) => abort!(meta, "spaad attributes can only be lists (`#[spaad::handler(...)]`) or paths (`#[spaad::handler]`)"),
        };

        if path.segments.first().unwrap().ident != "spaad" {
            continue;
        }
        if path.segments.len() != 2 {
            abort!(path, "spaad attributes should have a path of length 2 (i.e. `#[spaad::handler]`, not `#[spaad]`)");
        }

        let nested = match nested {
            Some(nested) => nested,
            None => {
                attributes.insert(path.segments.last().unwrap().ident.to_string(), None);
                continue;
            }
        };

        for entry in nested {
            let (key_path, value) = match entry {
                NestedMeta::Meta(Meta::NameValue(meta)) => (meta.path, meta.lit),
                _ => abort!(entry, "Inner should be name value meta"),
            };

            let ident = key_path
                .get_ident()
                .expect("Name value key should be ident")
                .clone();
            let ident_str = ident.to_string();

            let list_map = attributes
                .entry(path.segments.last().unwrap().ident.to_string())
                .or_insert_with(|| Some(HashMap::new()));

            list_map.as_mut().unwrap().insert(ident_str, (ident, value));
        }
    }

    attributes
}

fn lit_string_to_path(lit: &Lit) -> Path {
    let value = match lit {
        Lit::Str(string) => string.parse::<Path>(),
        _ => abort!(lit, "only string literals are allowed here"),
    };

    match value {
        Ok(p) => p,
        _ => abort!(lit, "value should be a valid type"),
    }
}

fn is_context_ref(ty_ref: &TypeReference) -> bool {
    let is_static = ty_ref
        .lifetime
        .as_ref()
        .map(|lif| lif.ident == "static")
        .unwrap_or(false);
    let is_context = ty_is_name(&ty_ref.elem, "Context");
    if !is_context && !is_static {
        abort!(ty_ref, "must be 'static or a reference to `Context<Self>`")
    }

    is_context
}

// I know, I apologize.
pub fn transform_method(impl_block: &ItemImpl, method: ImplItemMethod) -> proc_macro2::TokenStream {
    let name = get_name(&impl_block);
    let actor_name = get_actor_name(&impl_block);
    let act_ty_generics = impl_block.generics.split_for_impl().1;

    let attrs = parse_attributes(&method.attrs);

    let handler_attrs = attrs.get("handler");
    let is_handler = handler_attrs.is_some();

    if !is_handler {
        // matches checks for no receiver, i.e a static method
        return if matches!(method.sig.inputs.first(), Some(FnArg::Typed(_)) | None) {
            transform_static_methods(
                name,
                actor_name,
                attrs.contains_key("create"),
                attrs.contains_key("spawn"),
                method,
                &attrs,
                act_ty_generics,
            )
        } else {
            quote!()
        };
    }

    // TODO: throw an error for other attribute keys
    let reuse_msg = handler_attrs
        .unwrap()
        .as_ref()
        .and_then(|attrs| attrs.get("msg").map(|(_, lit)| lit.clone()));

    let ImplItemMethod {
        attrs,
        vis,
        mut sig,
        ..
    } = method;

    match sig.inputs.first_mut() {
        Some(FnArg::Typed(_)) | None => {
            abort!(
                sig.inputs,
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
                return !is_context_ref(ty_ref);
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
                quote!(#output),
                quote!(#output),
            )
        }
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
                    if is_context_ref(ty_ref) {
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
        })
        .collect();

    if reuse_msg.is_some() && call_inputs.len() != 1 {
        abort!(
            call_inputs[0],
            "you can only pass one arg when reusing a message"
        );
    }

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

    handler_generics.make_where_clause();

    if let Some(sig_clause) = sig.generics.where_clause.clone() {
        let handler_clause = handler_generics.where_clause.as_mut().unwrap();
        for where_clause in sig_clause.predicates {
            handler_clause.predicates.push(where_clause);
        }
    }

    let (handler_impl_generics, _, handler_where) = handler_generics.split_for_impl();

    let await_ = if sig.asyncness.is_some() {
        Some(quote!(.await))
    } else {
        None
    };

    let responder = quote!();

    let async_trait = quote!(#[::spaad::export::async_trait::async_trait]);

    if let Some(lit) = reuse_msg {
        let msg = lit_string_to_path(&lit);
        let msg_arg = call_inputs[0].clone();

        let handle = quote! {
            async fn handle(
                &mut self,
                #msg_arg: #msg,
                ctx: &mut ::spaad::export::xtra::Context<Self>,
            ) -> #result {
                self.#fn_name#fn_turbo(#(#call_inputs),*)#await_
            }
        };

        let handler = quote! {
            #async_trait
            #[allow(unused_variables)]
            impl#handler_impl_generics
                ::spaad::export::xtra::Handler<#msg>
            for #actor_name#act_ty_generics
                 #handler_where
            {
                #responder #handle
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

                #handler

                let f = self.addr.send(#msg_arg);
                async { f.await#handle_result }
            }

        }
    } else {
        let handle = quote! {
            async fn handle(
                &mut self,
                m: Msg#fn_ty_generics,
                ctx: &mut ::spaad::export::xtra::Context<Self>,
            ) -> #result {
                let Msg { #(#msg_members_destructured),* } = m;
                self.#fn_name#fn_turbo(#(#call_inputs),*)#await_
            }
        };

        let handler = quote! {
            #async_trait
            #[allow(unused_variables)]
            impl#handler_impl_generics
                ::spaad::export::xtra::Handler<Msg#fn_ty_generics>
            for #actor_name#act_ty_generics
                 #handler_where
            {
                #responder #handle
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

                struct Msg#fn_impl_generics #fn_where { #(#msg_members),* };

                impl#fn_impl_generics ::spaad::export::xtra::Message for Msg#fn_ty_generics
                    #fn_where
                {
                    type Result = #result;
                }

                #handler

                let f = self.addr.send(Msg#fn_turbo { #(#msg_members_destructured),* });
                async { f.await#handle_result }
            }
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
    attrs: &AttrMap,
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
        .map(|x| &**x)
        .collect();

    if has_create || has_spawn {
        transform_constructors(
            name,
            actor_name,
            method.clone(),
            attrs,
            impl_ty_generics,
            arg_inputs,
            inputs,
        )
    } else {
        let fn_name = &sig.ident;
        let method_ty_generics = sig.generics.split_for_impl().1;
        let method_turbo = method_ty_generics.as_turbofish();
        let act_turbo = impl_ty_generics.as_turbofish();
        let dot_await = if sig.asyncness.is_some() {
            Some(quote!(.await))
        } else {
            None
        };
        let ImplItemMethod { attrs, vis, .. } = method;

        quote! {
            #(#attrs)* #vis #sig {
                #actor_name#act_turbo::#fn_name#method_turbo(#(#inputs),*)#dot_await
            }
        }
    }
}

fn transform_constructors(
    name: &Ident,
    actor_name: proc_macro2::TokenStream,
    method: ImplItemMethod,
    attrs: &AttrMap,
    act_ty_generics: TypeGenerics,
    arg_inputs: &Punctuated<FnArg, Token![,]>,
    inputs: Vec<&Pat>,
) -> proc_macro2::TokenStream {
    let ImplItemMethod {
        attrs: method_attrs,
        vis,
        sig,
        ..
    } = method;
    let is_name = |ty, name| ty_is_name(ty, name);

    let name_string = name.to_string();
    if matches!(
        &sig.output,
        ReturnType::Type(_, ty) if !(is_name(ty, &name_string) || is_name(ty, "Self"))
    ) {
        abort!(
            sig.output,
            "functions annotated with `spawn` or `create` must return `Self`"
        );
    }

    let mut spawn: Option<TokenStream> = None;

    if let Some(attr) = attrs.get("spawn") {
        let spawner = attr.as_ref().and_then(|attr| get_spawner(attr));

        let mut new_generics = sig.generics.clone();

        if spawner.is_none() {
            let param = quote!(ActorSpawner: ::spaad::export::xtra::spawn::Spawner);
            let param: GenericParam = syn::parse2(param).unwrap();
            new_generics.params.push(param);
        }

        let (impl_generics, _ty_generics, where_clause) = new_generics.split_for_impl();
        let act_turbo = act_ty_generics.as_turbofish();
        let (_, old_ty_generics, _) = sig.generics.split_for_impl();
        let fn_turbo = old_ty_generics.as_turbofish();
        let act_fn_name = &sig.ident;

        let fn_name = get_ctor_name(&sig, attr, true);

        let trailing = if !arg_inputs.is_empty() && !arg_inputs.trailing_punct() {
            Some(quote!(,))
        } else {
            None
        };
        let spawner_arg = if spawner.is_some() {
            None
        } else {
            Some(quote!(actor_spawner: &mut ActorSpawner,))
        };
        let spawner_arg = quote!(#trailing#spawner_arg);

        let spawner_ref = if let Some(s) = spawner {
            quote!(&mut #s)
        } else {
            quote!(actor_spawner)
        };

        spawn = Some(quote! {
            #(#method_attrs)* #vis fn #fn_name#impl_generics(
                #arg_inputs
                #spawner_arg
            ) -> Self #where_clause {
                use ::spaad::export::xtra::prelude::*;
                let act = #actor_name#act_turbo::#act_fn_name#fn_turbo(#(#inputs),*);
                let addr = act.create(::std::option::Option::None).spawn(#spawner_ref);
                #name { addr }
            }
        });
    };

    let mut create = None;
    if let Some(attr) = attrs.get("create") {
        let (impl_generics, ty_generics, where_clause) = sig.generics.split_for_impl();
        let act_turbo = act_ty_generics.as_turbofish();
        let fn_turbo = ty_generics.as_turbofish();
        let act_fn_name = &sig.ident;

        let fn_name = get_ctor_name(&sig, attr, false);
        create = Some(quote! {
            #(#method_attrs)* #vis fn #fn_name#impl_generics(
                #arg_inputs
            ) -> ::spaad::export::xtra::ActorManager<#actor_name#act_ty_generics>
                #where_clause
            {
                use ::spaad::export::xtra::prelude::*;
                let act = #actor_name#act_turbo::#act_fn_name#fn_turbo(#(#inputs),*);
                let mgr = act.create(::std::option::Option::None);
                mgr
            }
        })
    };

    return quote!(#spawn #create);
}

fn get_ctor_name(
    sig: &Signature,
    attr: &Option<HashMap<String, (Ident, Lit)>>,
    spawn: bool,
) -> Ident {
    let fn_name = sig.ident.clone();

    let attr_arg_map = match attr {
        Some(map) => map,
        None => return fn_name,
    };

    for (key, (ident, lit)) in attr_arg_map {
        match &**key {
            "rename" => match lit {
                Lit::Str(lit) => return format_ident!("{}", lit.value()),
                _ => abort!(lit, "Expected rename target to be a string"),
            },
            "spawner" if spawn => {}
            _ if spawn => abort!(
                ident,
                "Only two valid arguments here: `rename` and `spawner`"
            ),
            _ => abort!(ident, "Only one valid argument here: `rename`"),
        }
    }

    fn_name
}

fn get_spawner(attr: &HashMap<String, (Ident, Lit)>) -> Option<TokenStream2> {
    if let Some((_, spawner)) = attr.get("spawner") {
        let spawner = match spawner {
            Lit::Str(lit) => match &*lit.value().to_lowercase() {
                "tokio" => quote!(::spaad::export::xtra::spawn::Tokio::Global),
                "async_std" => quote!(::spaad::export::xtra::spawn::AsyncStd),
                "smol" => quote!(::spaad::export::xtra::spawn::Smol::Global),
                "wasm_bindgen" => quote!(::spaad::export::xtra::spawn::WasmBindgen),
                _ => abort!(
                    spawner,
                    "Expected one of \"tokio\", \"async_std\", \"smol\", \"wasm_bindgen\""
                ),
            },
            _ => abort!(spawner, "Expected spawner to be a string"),
        };
        return Some(spawner);
    }

    None
}
