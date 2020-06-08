//
// #[spaad::entangled]
// struct Printer {
//     times: usize,
// }
//
// #[spaad::entangled]
// impl Actor for Printer {}
//
// #[spaad::entangled]
// impl Printer {
//     fn new() -> Self {
//         Self { times: 0 }
//     }
//
//     async fn print(&mut self, string: String) {
//         self.times += 1;
//         println!("Printing {}. Printed {} times so far.", string, self.times);
//     }
// }

#[derive(Clone)]
struct X<T: 'static + Send + Clone, A>
where
    A: 'static + Send + Clone,
{
    addr: ::spaad::export::xtra::Address<__XActor::X<T, A>>,
}
impl<T: 'static + Send + Clone, A> X<T, A>
where
    A: 'static + Send + Clone,
{
    fn into_address(self) -> ::spaad::export::xtra::Address<__XActor::X<T, A>> {
        self.addr
    }
    fn address(&self) -> &::spaad::export::xtra::Address<__XActor::X<T, A>> {
        &self.addr
    }
}
#[doc(hidden)]
#[allow(non_snake_case)]
mod __XActor {
    #[derive(Clone)]
    pub struct X<T: 'static + Send + Clone, A>
    where
        A: 'static + Send + Clone,
    {
        pub(super) t: T,
        pub(super) a: A,
        pub(super) b: i32,
    }
}
impl<T: 'static + Send + Clone, A> xtra::Actor for __XActor::X<T, A>
where
    A: 'static + Send + Clone,
{
    fn started(&mut self, _: &mut xtra::Context<Self>) {}
}
mod __impl0 {
    use super::*;
    use crate::X;
    impl<T: 'static + Send + Clone, A> __XActor::X<T, A>
    where
        A: 'static + Send + Clone,
    {
        #[spaad::spawn]
        fn new(t: T, a: A) -> X<T, A> {
            X { t, a, b: 0 }
        }
        #[spaad::handler]
        async fn foo(&mut self, mut h: f64, ctx: &mut xtra::Context<Self>) {
            self.b += 1;
            h += 1.0;
            println!("hello {}", h);
            println!("b = {}", self.as_ref());
            self.blabla().await;
        }
        #[spaad::handler]
        async fn bar(&mut self) -> Result<(), xtra::Disconnected> {
            self.b -= 1;
            println!("goodbye");
            Ok(())
        }
        #[spaad::handler]
        async fn blabla(&mut self) {
            println!("middle!");
            self.not_a_handler().await;
        }
        async fn not_a_handler(&mut self) {
            println!("almost there!");
            self.not_async();
        }
        #[spaad::handler]
        fn not_async(&self) {
            println!("one more!!");
        }
    }
    impl<T: 'static + Send + Clone, A> X<T, A>
    where
        A: 'static + Send + Clone,
    {
        #[spaad::spawn]
        fn new(t: T, a: A) -> Self {
            use ::spaad::export::xtra::prelude::*;
            let act = __XActor::X::new(t, a);
            let addr = act.spawn();
            X { addr }
        }
        #[allow(unused_mut)]
        #[spaad::handler]
        fn foo(&self, mut h: f64) -> impl std::future::Future<Output = ()> {
            use ::spaad::export::xtra::prelude::*;
            struct Msg {
                h: f64,
            };
            impl ::spaad::export::xtra::Message for Msg {
                type Result = ();
            }
            #[::spaad::export::async_trait::async_trait]
            #[allow(unused_variables)]
            impl<T: 'static + Send + Clone, A> Handler<Msg> for __XActor::X<T, A>
            where
                A: 'static + Send + Clone,
            {
                async fn handle(&mut self, m: Msg, ctx: &mut Context<Self>) -> () {
                    let Msg { h } = m;
                    self.foo(h, ctx).await
                }
            }
            let f = self.addr.send(Msg { h });
            async { f.await.expect("actor disconnected") }
        }
        #[allow(unused_mut)]
        #[spaad::handler]
        fn bar(&self) -> impl std::future::Future<Output = Result<(), xtra::Disconnected>> {
            use ::spaad::export::xtra::prelude::*;
            struct Msg {};
            impl ::spaad::export::xtra::Message for Msg {
                type Result = Result<(), xtra::Disconnected>;
            }
            #[::spaad::export::async_trait::async_trait]
            #[allow(unused_variables)]
            impl<T: 'static + Send + Clone, A> Handler<Msg> for __XActor::X<T, A>
            where
                A: 'static + Send + Clone,
            {
                async fn handle(
                    &mut self,
                    m: Msg,
                    ctx: &mut Context<Self>,
                ) -> Result<(), xtra::Disconnected> {
                    let Msg {} = m;
                    self.bar().await
                }
            }
            let f = self.addr.send(Msg {});
            async { f.await.and_then(|x| x) }
        }
        #[allow(unused_mut)]
        #[spaad::handler]
        fn blabla(&self) -> impl std::future::Future<Output = ()> {
            use ::spaad::export::xtra::prelude::*;
            struct Msg {};
            impl ::spaad::export::xtra::Message for Msg {
                type Result = ();
            }
            #[::spaad::export::async_trait::async_trait]
            #[allow(unused_variables)]
            impl<T: 'static + Send + Clone, A> Handler<Msg> for __XActor::X<T, A>
            where
                A: 'static + Send + Clone,
            {
                async fn handle(&mut self, m: Msg, ctx: &mut Context<Self>) -> () {
                    let Msg {} = m;
                    self.blabla().await
                }
            }
            let f = self.addr.send(Msg {});
            async { f.await.expect("actor disconnected") }
        }
        #[allow(unused_mut)]
        #[spaad::handler]
        fn not_async(&self) -> impl std::future::Future<Output = ()> {
            use ::spaad::export::xtra::prelude::*;
            struct Msg {};
            impl ::spaad::export::xtra::Message for Msg {
                type Result = ();
            }
            #[::spaad::export::async_trait::async_trait]
            #[allow(unused_variables)]
            impl<T: 'static + Send + Clone, A> Handler<Msg> for __XActor::X<T, A>
            where
                A: 'static + Send + Clone,
            {
                async fn handle(&mut self, m: Msg, ctx: &mut Context<Self>) -> () {
                    let Msg {} = m;
                    self.not_async()
                }
            }
            let f = self.addr.send(Msg {});
            async { f.await.expect("actor disconnected") }
        }
    }
}

impl<T: 'static + Send + Clone, A> AsRef<i32> for __XActor::X<T, A>
where
    A: 'static + Send + Clone,
{
    fn as_ref(&self) -> &i32 {
        &self.b
    }
}
