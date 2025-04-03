pub type Print = fn(String);
pub fn std_print(s: String) {
    println!("{}", s)
}

pub enum Stdlib {
    Print(Print),
}
