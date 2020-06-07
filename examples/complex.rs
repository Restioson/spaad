#[spaad::entangled]
#[derive(Clone)]
struct X<T: 'static + Send + Clone, A> where A: 'static + Send + Clone {
    t: T,
    a: A,
    b: i32,
}

#[spaad::entangled]
impl<T: 'static + Send + Clone, A> xtra::Actor for X<T, A>
    where A: 'static + Send + Clone
{
    fn started(&mut self, _: &mut xtra::Context<Self>) {

    }
}

#[spaad::entangled]
impl<T: 'static + Send + Clone, A> X<T, A>
    where A: 'static + Send + Clone
{
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
        println!("hello {}", h);
        println!("b = {}", self.as_ref()); // calling trait method on self
        self.blabla().await; // await needed - we are calling the async function itself.
        ctx.notify_immediately(Notification); // interop with normal xtra handlers
    }

    async fn bar(&mut self) -> Result<(), xtra::Disconnected> {
        self.b -= 1;
        println!("goodbye");
        Ok(())
    }

    async fn blabla(&mut self) {
        println!("middle!");
    }
}

struct Notification;
impl xtra::Message for Notification {
    type Result = ();
}

#[spaad::entangled]
#[async_trait::async_trait]
impl<T: 'static + Send + Clone, A> xtra::Handler<Notification> for X<T, A>
    where A: 'static + Send + Clone
{
    async fn handle(&mut self, _: Notification, ctx: &mut xtra::Context<Self>) {
        println!("stopping");
        ctx.stop();
    }
}

#[spaad::entangled]
impl<T: 'static + Send + Clone, A> AsRef<i32> for X<T, A>
    where A: 'static + Send + Clone
{
    fn as_ref(&self) -> &i32 {
        &self.b
    }
}

#[tokio::main]
async fn main() {
    let x: X<u32, u32> = X::<u32, u32>::spawn(1, 2);
    let x: X<u32, u32> = x;
    x.foo(1.0).await;
    assert!(x.bar().await.is_err()); // disconnected, so we assert that it returned error
}
