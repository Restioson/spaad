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

Since spaad uses nightly features such as GATs and Type Alias Impl Trait, those features will need to be enabled. These
are the same features generally required for xtra.

spaad exports one thing: the proc macro `spaad::entangled`. This is used as an attribute on the actor struct definition,
`Actor` implementation, and on an impl block in which the handler functions are used. For example:

```rust
#![feature(type_alias_impl_trait, generic_associated_types)]

#[spaad::entangled]
struct Printer {
    times: usize,
}

#[spaad::entangled]
impl Actor for Printer {
    fn started(&mut self, _ctx: &mut Context<Self>) {
        println!("Actor started!");
    }
}

#[spaad::entangled]
impl Printer {
    // New is a special case: if it is present, create() and spawn() functions are emitted too
    fn new() -> Self {
        // You must use Self when referring to the type itself, as it is internally renamed something else. 
        // This allows for better IDE support.
        Self { times: 0 }
    }

    async fn print(&mut self, string: String) {
        self.times += 1;
        println!("Printing {}. Printed {} times so far.", string, self.times);
    }
}
```

It is important to note that the `new` function is a special case. If it is present, the proc macro will also emit a
`create` method for the actor wrapper. It can take arguments. If the `with-tokio-0_2` or `with-async_std-1_0` features
are enabled, then it will also emit a `spawn` method. These are both very similar to the `xtra::Actor` methods of the
same names.

Messages can then be sent to actors as such:
```rust
my_actor.print().await;
```

If you do not want to `await` for the message to complete processing, you can do the following:
```rust
let _ = my_actor.print(); // Binding to avoid #[must_use] warning on Future
```

The proc macro will create the given struct (in the example's case, `Printer`). This is just a wrapper for an actor
address to the actual structure, which is strictly internal and cannot be interacted with except by sending messages.

For a more complex example, such as handling the actor's disconnection and taking `Context` in a handler, see the
documentation or `complex.rs` in the examples directory.
