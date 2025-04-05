#[derive(Debug, Clone)]
pub enum RustBinding {
    Print(Print),
}

pub type Print = fn(Vec<String>);
pub fn std_rust_print(s: Vec<String>) {
    println!("{}", s.join(" "))
}
