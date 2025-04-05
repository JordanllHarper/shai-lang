#[derive(Debug, Clone)]
pub enum Stdlib {
    Print(Print),
}

pub type Print = fn(Vec<String>);
pub fn std_print(s: Vec<String>) {
    println!("{}", s.join(""))
}
