#![cfg_attr(doc, allow(unused_braces))]

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::*;

mod entangle;

/// The main item of the crate. This is a proc macro used as an attribute on the actor struct
/// definition, `Actor` implementation, and on an impl block in which the handler functions are used.
///
/// ## Example
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
///     #[spaad::spawn]
///     fn new() -> Printer {
///         Printer { times: 0 }
///     }
///
///     #[spaad::handler]
///     async fn print(&mut self, string: String) {
///         self.times += 1;
///         println!("Printing {}. Printed {} times so far.", string, self.times);
///     }
/// }
/// ```
///
/// ## Constructors
/// To emit a constructor for an actor, the `#[spaad::spawn]` or `#[spaad::create]` attributes can
/// be used. The `spawn` macro will emit a method that constructs the actor with the given arguments
/// and spawns it onto whichever runtime `spaad` is set to use (currently, tokio, async_std, or
/// wasm-bindgen at your option). It is analagous to `Actor::spawn`. Similarly, `create` will
/// construct the actor, and return the address of the actor and its manager, ready to be spawned
/// onto any runtime. You can even have both on one function with `rename`:
///
/// ```rust,ignore
/// #[spaad::spawn]
/// #[spaad::create(rename = "create")]
/// fn new(x: u32) -> MyActor {
///     MyActor { x }
/// }
/// ```
///
/// This will cause a `create` function to be emitted, as well as a a spawn function named `new`.
///
/// ## Sending Messages
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
/// ## Handling disconnection
/// The methods to send messages will panic if the actor is disconnected. If you want to manually
/// handle this error, make the return type of the handler function `Result<T, xtra::Disconnected>`.
/// The type must be named `Context` - it cannot be renamed by re-importing. If you want to access
/// the actor cotnext add an argument to the function with `&mut Context<Self>` as the type.
/// Similarly, the type must be named `Context` - it cannot be renamed by re-importing.
///
/// ## Implementations in other modules
/// To implement something on an actor in a module other than where it is declared, you will need
/// to refer to it either by its fully-qualified path (e.g `crate::actor::MyActor`) or a local path
/// (e.g `super::MyActor`) when writing the self-type. So, instead of writing this:
///
/// ```rust,ignore
/// use super::MyActor;
/// #[spaad::entangled]
/// impl AsRef<i32> for MyActor { /* ... */ }
/// ```
///
/// you must write this:
///
/// ```rust,ignore
/// #[spaad::entangled]
/// impl AsRef<i32> for super::MyActor { /* ... */ }
/// ```
///
/// This is a limitation due to how the macro expands, and should be resolved when there is support
/// for inherent-impl type aliases (see [rust/60471](https://github.com/rust-lang/rfcs/issues/1697)).
/// This is currently blocked on lazy normalization.
#[proc_macro_error::proc_macro_error]
#[proc_macro_attribute]
pub fn entangled(_args: TokenStream, input: TokenStream) -> TokenStream {
    entangle::entangle(input)
}

// The below attributes are just markers, so they just strip themselves from the output and output
// the rest of the function.

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
/// return the type of the actor, either as `Self` or by its name.
///
/// ## Arguments
///
/// This macro can be passed what to rename the method in the form of `rename = "{new name}"`.
/// This is most useful when generating both a create and spawn method from the same constructor.
///
/// ## Usage
///
/// ```ignore
/// #[spaad::spawn]
/// fn new(some: Thing) -> MyActor {
///     MyActor { some }
/// }
///
/// #[spaad::spawn(rename = "something_else")] // Emits a method called `something_else`
///  fn spawn2(some: Thing) -> MyActor {
///      MyActor { some }
///  }
/// ```
#[proc_macro_attribute]
pub fn spawn(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as ImplItemMethod);
    TokenStream::from(quote!(#input))
}

/// This marks a function as the method that should be used to create the actor. It emits a method
/// which returns a tuple of the address to the actor and its `ActorManager`.
///
/// ## Arguments
///
/// This macro can be passed what to rename the method in the form of `rename = "{new name}"`.
/// This is most useful when generating both a create and spawn method from the same constructor.
///
/// ## Usage
///
/// ```ignore
/// #[spaad::create]
/// fn create(some: Thing) -> MyActor {
///     MyActor { some }
/// }
///
/// #[spaad::create(rename = "something_else")] // Emits a method called `something_else`
/// fn create2(some: Thing) -> MyActor {
///     MyActor { some }
/// }
/// ```
#[proc_macro_attribute]
pub fn create(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as ImplItemMethod);
    TokenStream::from(quote!(#input))
}
