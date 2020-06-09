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
    // Declared mut because intellij wants it to be - it doesn't actually have to be mut.
    // It appears that rust-analyzer does not have this issue, for whatever reason.
    #[allow(unused_mut)]
    let mut printer = Printer::new();

    loop {
        printer.print("hello".to_string()).await;
    }
}
