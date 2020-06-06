//! Two examples of code generated from the `spaad::entangle` attribute, showcasing the public API.
//! API stability is not guaranteed whatsoever for this module.

// NOTE TO CODE DELVERS: cargo doc does not like GAT + TAIT together, so the bodies of some
// functions are just replaced with the minimum to make it compile and document.
// This is manually edited from the formatted, generated code.

#![feature(type_alias_impl_trait, generic_associated_types)]

pub mod simple {
    #[derive(Clone)]
    pub struct Printer {
        addr: xtra::Address<__PrinterActor>,
    }
    #[doc(hidden)]
    pub struct __PrinterActor {
        times: usize,
    }
    impl xtra::Actor for __PrinterActor {}
    impl __PrinterActor {
        fn new() -> Self {
            Self { times: 0 }
        }
        async fn print(&mut self, string: String) {
            self.times += 1;
            println!("Printing {}. Printed {} times so far.", string, self.times);
        }
    }
    impl Printer {
        #[allow(dead_code)]
        pub fn spawn() -> Self {
            panic!()
        }
        #[allow(dead_code)]
        pub fn create() -> (Self, xtra::ActorManager<__PrinterActor>) {
            panic!()
        }
        #[allow(unused_mut)]
        pub fn print(&self, string: String) -> impl std::future::Future<Output = ()> {
            async {()}
        }
    }
}

pub mod complex {
    #[derive(Clone)]
    pub struct X<T: 'static + Send + Clone, A: 'static + Send + Clone> {
        addr: xtra::Address<__XActor<T, A>>,
    }
    #[derive(Clone)]
    #[doc(hidden)]
    pub struct __XActor<T: 'static + Send + Clone, A: 'static + Send + Clone> {
        t: T,
        a: A,
        b: i32,
    }
    impl<T: 'static + Send + Clone, A: 'static + Send + Clone> xtra::Actor for __XActor<T, A> {}
    impl<T: 'static + Send + Clone, A: 'static + Send + Clone> __XActor<T, A> {
        pub fn new(t: T, a: A) -> Self {
            Self { t, a, b: 0 }
        }
        pub async fn foo(&mut self, mut h: f64, ctx: &mut xtra::Context<Self>) {}
        pub async fn bar(&mut self) -> Result<(), xtra::Disconnected> {
            Ok(())
        }
    }
    impl<T: 'static + Send + Clone, A: 'static + Send + Clone> X<T, A> {
        #[allow(dead_code)]
        pub fn spawn(t: T, a: A) -> Self {
            panic!()
        }
        #[allow(dead_code)]
        pub fn create(t: T, a: A) -> (Self, xtra::ActorManager<__XActor<T, A>>) {
            panic!()
        }
        #[allow(unused_mut)]
        pub fn foo(&self, mut h: f64) -> impl std::future::Future<Output = ()> {
            async {()}
        }
        #[allow(unused_mut)]
        pub fn bar(&self) -> impl std::future::Future<Output = Result<(), xtra::Disconnected>> {
            async {Ok(())}
        }
    }
}
