#![feature(type_alias_impl_trait, generic_associated_types)]

use tokio;

#[spaad::entangled]
#[derive(Clone)]
struct X<T: 'static + Send + Clone, A: 'static + Send + Clone> {
    t: T,
    a: A,
    b: i32,
}

#[spaad::entangled]
impl<T: 'static + Send + Clone, A: 'static + Send + Clone> xtra::Actor for X<T, A> {
    fn started(&mut self, _: &mut xtra::Context<Self>) {

    }
}

#[spaad::entangled]
impl<T: 'static + Send + Clone, A: 'static + Send + Clone> X<T, A> {
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
