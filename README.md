# spaad
_**Sp**ooky **a**ction **a**t a **d**istance_

spaad is a crate which removes the vast majority of boilerplate in [xtra](https://github.com/Restioson/xtra), a tiny
actor framework. It does this through a proc macro: `spaad::entangled`. The effect is that both writing a message handler
and calling it looks nigh-identical to a traditional method call.

For instance, a handler looks like this:
```rust
#[spaad::entangled]
impl MyActor {
    async fn print(&mut self) { /* ... */ }
}
```

and is called like this:

```rust
my_actor.print().await;
```

## Usage

The proc macro `spaad::entangled` is the core item of spaad. It creates the messages and `Handler` implementations for
each handler from its signature, as well as a struct wrapping the address, which has ergonomic function names for sending messages.
The end result is that it looks as though no actors are involved to both the caller and callee. The name of the crate is
a cheeky reference to what Einstein called quantum entanglement - "_**sp**ooky **a**ction **a**t a **d**istance_" - since
in quantum entanglement it also appears that one particle's state is "magically" changed.

This macro is used as an attribute on the actor struct definition and its impl blocks.

## Example 
```rust
use xtra::prelude::*;

#[spaad::entangled]
pub struct Printer {
    times: usize,
}

#[spaad::entangled]
impl Actor for Printer {}

#[spaad::entangled]
impl Printer {
    #[spaad::spawn]
    pub fn new() -> Self {
        Printer { times: 0 }
    }

    #[spaad::handler]
    pub fn print(&mut self, to_print: String) {
        self.times += 1;
        println!(
            "Printing {}. Printed {} times so far.",
            to_print, self.times
        );
    }
}

#[tokio::main]
async fn main() {
    let printer = Printer::new();

    loop {
        printer.print("hello".to_string()).await;
    }
}
```

The generated `Printer` type does not, in fact, contain all members of the strucy, but rather its address.
The actual structure is strictly internal and cannot be interacted with except by sending messages or from inside its
impl blocks. When referred to inside of impl blocks as a type, `Self` must be used, as it will be renamed.

It is important to note that the `new` function is a special case. If it is present, the proc macro will also emit a
`create` method for the actor wrapper, corresponding to `Actor::create`. It can take arguments. If the `with-tokio-0_2`
or `with-async_std-1_0` features are enabled, then it will also emit a `new` method, which corresponds to `Actor::spawn`.

If you do not want to `await` for the message to complete processing, you can do the following:
```rust
let _ = my_actor.print(); // Binding to avoid #[must_use] warning on Future
```

For a more complex example, such as handling the actor's disconnection and taking `Context` in a handler, see the
documentation or `complex.rs` in the examples directory. To see the generated code, run `cargo +nightly doc` in the 
`example_generated` folder.

## Advantages

- More ergonomic and concise.
- IDE support. It is possible in some IDEs (only tested on IntelliJ IDEA with the Rust plugin) to jump to definition.
  This is only partial, as in some cases the IDE does not understand the change in the mutability of the `self` parameter
  (only `&self` is required when sending a message, but it looks as though it is declared `&mut self`).
- Can use nightly API with less chance of UB (by removing an opportunity for UB in GATs), and in a completely transparent
  manner. 

## Disadvantages

- Similar caveat to xtra itself: immaturity.
- IDE support is not full. In some cases, there can be issues. This could be resolved by proc macro expansion, but that
  appears to be a way off. It appears that Rust Analyzer handles this slightly better than IntelliJ Rust, though this
  may change.

## Nightly API

In order to enable the xtra nightly API, disable the default `stable` feature in your `Cargo.toml`.