#![cfg_attr(doc, allow(unused_braces))]
#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::*;

mod entangle;

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
pub fn entangled(_args: TokenStream, input: TokenStream) -> TokenStream {
    let x = entangle::entangle(input);
    println!("{}", x);
    x
}

/// This marks a function as a handler for a type of message, exposing it to external callers.
///
/// ## Usage
///
/// ```ignore
/// #[spaad::handler]
/// async fn do_something() {/* ... */}
/// ```
#[proc_macro_attribute]
pub fn handler(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as ImplItemMethod);
    TokenStream::from(quote!(#input))
}

/// This marks a function as the method that should be used to create and spawn the actor. It must
/// return the type of the actor, either as `Self` or by its name. However, do note that when using
/// a struct initializer, **it must be referred to by `Self`.**
///
/// ## Usage
///
/// ```ignore
/// #[spaad::spawn]
/// fn new(some: Thing) -> Self {
///     Self { some }
/// }
/// ```
#[proc_macro_attribute]
pub fn spawn(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as ImplItemMethod);
    TokenStream::from(quote!(#input))
}
