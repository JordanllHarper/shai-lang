# SHAI ☕︎

Shai is an interpreted scripting language built in Rust. It aims to be simple, familiar and to get out of your way when building scripts.

Check out [examples](examples/) for a peek at the syntax of Shai, or [the currently implemented features](DEF_OF_DONE.md) and what future improvements could look like.

> [!NOTE]
> This project is an MVP and so is not stable for use.

## Installation and Usage

### Binary release

To install, you have 2 options. The first is to install the binary [from the releases](https://github.com/JordanllHarper/shai-lang/releases).

You can then invoke using `shai` followed by your script name.

```bash
$ shai hello.shai
```

> [!NOTE]
> You can either invoke it directly or add it to your `$PATH`. 

### From source

The other is to build from source. The language uses the Programming language [Rust](https://www.rust-lang.org/), so follow the installation instructions there. 

Clone the repository and then invoke the interpreter:

```bash
# With debug information
$ cargo run hello.shai

# Or without debug information
$ cargo run --release hello.shai

# Or build a binary - target/release/shai
$ cargo build --release
```
