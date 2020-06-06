#![feature(type_alias_impl_trait, generic_associated_types)]

#[spaad::entangled]
struct Printer {
    times: usize,
}

#[spaad::entangled]
impl Actor for Printer {}

#[spaad::entangled]
impl Printer {
    fn new() -> Self {
        Self { times: 0 }
    }

    async fn print(&mut self, string: String) {
        self.times += 1;
        println!("Printing {}. Printed {} times so far.", string, self.times);
    }
}

#[tokio::main]
async fn main() {
    let printer: Printer = Printer::spawn();
     loop {
        printer.print("hello".to_string()).await;
     }
}

