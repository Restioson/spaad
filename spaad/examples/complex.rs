#[spaad::entangled]
#[derive(Clone)]
pub struct X<T: 'static + Send + Clone, A>
where
    A: 'static + Send + Clone,
{
    t: T,
    a: A,
    b: i32,
}

#[spaad::entangled]
#[async_trait::async_trait]
impl<T: 'static + Send + Clone, A> xtra::Actor for X<T, A>
where
    A: 'static + Send + Clone,
{
    async fn started(&mut self, _: &mut xtra::Context<Self>) {}
}

#[spaad::entangled]
impl<T: 'static + Send + Clone, A> X<T, A>
where
    A: 'static + Send + Clone,
{
    #[spaad::spawn(spawner = "tokio")]
    #[spaad::create(rename = "create")]
    pub fn new<Y: Into<i32>>(t: T, a: A, y: Y) -> X<T, A> {
        X { t, a, b: y.into() }
    }

    #[spaad::spawn]
    pub fn new_no_spawn<Y: Into<i32>>(t: T, a: A, y: Y) -> X<T, A> {
        X { t, a, b: y.into() }
    }

    #[spaad::handler]
    pub async fn foo(&mut self, mut h: f64, ctx: &mut xtra::Context<Self>) {
        self.b += 1;
        h += 1.0;
        println!("hello {}", h);
        println!("b = {}", self.as_ref()); // calling trait method on self
        self.blabla().await; // await needed - we are calling the async function itself.
        ctx.notify(impl_somewhere_else::Notification); // interop with normal xtra
    }

    #[spaad::handler]
    pub async fn bar(&mut self) -> Result<(), xtra::Disconnected> {
        self.b -= 1;
        println!("goodbye");
        Ok(())
    }

    #[spaad::handler]
    pub async fn blabla(&mut self) {
        println!("middle!");
        self.not_a_handler().await;
    }

    #[spaad::handler]
    pub async fn handle_generically<I: Into<i32> + Send + 'static>(&mut self, i: I) {
        self.b = i.into();
    }

    pub async fn not_a_handler(&mut self) {
        println!("almost there!");
        self.not_async();
    }

    pub async fn another_non_handler<'a>(&mut self, s: &'a str) -> &'a str {
        s
    }

    #[spaad::handler]
    pub fn not_async(&self) {
        println!("one more!!");
    }

    pub fn associated_func() -> i32 {
        0
    }

    pub fn another_assoc_func(s: &str) -> &str {
        s
    }
}

pub mod impl_somewhere_else {
    pub struct Notification;
    impl xtra::Message for Notification {
        type Result = ();
    }

    #[spaad::entangled]
    #[async_trait::async_trait]
    impl<T: 'static + Send + Clone, A> xtra::Handler<Notification> for super::X<T, A>
    where
        A: 'static + Send + Clone,
    {
        async fn handle(&mut self, _: Notification, ctx: &mut xtra::Context<Self>) {
            println!("stopping");
            ctx.stop();
        }
    }
}

#[spaad::entangled]
impl<T: 'static + Send + Clone, A> AsRef<i32> for X<T, A>
where
    A: 'static + Send + Clone,
{
    fn as_ref(&self) -> &i32 {
        &self.b
    }
}

#[tokio::main]
async fn main() {
    #[allow(unused_mut)] // for intellij we set as mut :)
    let mut x = X::<u32, u32>::new(1, 2, 0i32);
    let (addr, fut) = X::<u32, u32>::create::<i32>(1, 2, 0i32).run();
    tokio::spawn(fut);
    let _x2: X::<u32, u32> = addr.into();
    let _x3 = X::<u32, u32>::new_no_spawn(1, 2, 0i32, &mut xtra::spawn::Tokio::Global);
    let _ = x.handle_generically(1i32); // ignore result
    x.foo(1.0).await;
    assert!(x.bar().await.is_err()); // disconnected, so we assert that it returned error
}
