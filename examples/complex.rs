#![feature(type_alias_impl_trait)]
#![feature(generic_associated_types)]
#![allow(dead_code, unused_mut)]

use tokio;

#[spaad::entangled]
struct X<T: 'static + Send, A: 'static + Send> {
    t: T,
    a: A,
    b: i32,
}

#[spaad::entangled]
impl<T: 'static + Send, A: 'static + Send> xtra::Actor for X<T, A> {
    fn started(&mut self, _: &mut xtra::Context<Self>) {

    }
}

#[spaad::entangled]
impl<T: 'static + Send, A: 'static + Send> X<T, A> {
    fn new(t: T, a: A) -> Self {
        Self {
            t,
            a,
            b: 0,
        }
    }

    async fn foo(&mut self, mut h: f64, ctx: &mut xtra::Context<Self>) {
        self.b += 1;
        h += 1.0;
        ctx.stop();
        println!("hello {}", h);
    }

    async fn bar(&mut self) -> Result<(), xtra::Disconnected> {
        self.b -= 1;
        println!("goodbye");
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let x: X<u32, u32> = X::<u32, u32>::spawn(1, 2);
    let mut x: X<u32, u32> = x;
    x.foo(1.0).await;
    assert!(x.bar().await.is_err()); // disconnected, so we assert that it returned error
}
