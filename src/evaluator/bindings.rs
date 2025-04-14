use crate::dbg;

#[derive(Debug, Clone)]
pub enum RustBinding {
    Print(Print),
    Len,
}

pub type Print = fn(Vec<String>);
pub fn std_rust_print(s: Vec<String>) {
    let s = s.join(" ");
    dbg!("STDOUT: {}", &s);
    println!("{}", s)
}
