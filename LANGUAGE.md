# The Shai-Lang Language

## Introduction

Shai is an expression based scripting language designed for writing utilities in a terminal environment. It aims to be familiar to reduce context switching when coming from a large project to writing helper scripts.

It takes inspiration from the following languages:

- Bash + Nushell for acting as a Shell scripting language.
- Go for it's development speed.
- Python + Lua for dynamic typing and preference for mechanisms over contracts.
- Rust + Kotlin for it's expresssion based systems.

Below is an overview of the language.

## Comments

```
// I won't get executed

/*
    I am multiple lines!
    Look
    at
    me
    go
*/
```

## Hello World

Let's get going with hello world!

```
print "Hello, World!"

print "Hi, Mom!" // puts text on a new line

print "Hello" "Everyone" // separate out the text as you need
```

## Storing variables and Data types

The supported data types of the language are:

- `bool`
- `int`
- `string`
- `float`
- `char`
- `arr`
- `dict`

```

is_cool = true // bool

my_num = 5 // int

name = "Bob" // string

pi = 3.14 // float

my_initial = 'J' // char

my_empty_array = [] // array (empty)

my_populated_array = [1, 2, 3] // array (populated)

my_empty_dict = {:} // dict (empty)

my_populated_dict = { // dict (populated)
    "hello": "world"
}

```

## If else expressions

Shai allows you to branch based on a condition that evaluates to true or false:

```
x = true

if x {  // x evaluates to true so this will run
    print "You are awesome"
}

x = false
if x { // x now evaluates to false so this will not run
    print ":("
} else { // x is false so this WILL
    print "...still awesome"
}

```

## Loops

Shai supports various methods of iterating

```

// iterates forever
while true {
    print "forever"
}

// for these kinds of loops, you can omit the condition entirely
while {
    print "still forever"
}

// you can loop while a certain condition is true. When it is false, the loop will end
x = 5
while x > 0 {
    print x
    x -= 1 // decrement each loop
}
// output:
// 5
// 4
// 3
// 2
// 1

// you can also specify the amount of times you'd like to use by using a for loop.
// Shai supports "ranges", which represents a range of numbers from one point to another

for i in 0..5 {
    print i
}

// output:
// 0
// 1
// 2
// 3
// 4

// To include the last number in the iteration, add an '='
for i in 0..=5 {
    print i
}

// output:
// 0
// 1
// 2
// 3
// 4
// 5

// for loops can be used to iterate over a collection such as an array or dictionary

my_array = [1, 2, 3]

for i in my_array {
    print i
}

// output:
// 1
// 2
// 3

my_dict = {
    "hello": "world"
    "hi": "mom"
}

for key, value in my_dict {
    print key
    print value
}

// output:
// hello
// world
// hi
// mom

```

## Getting values out of arrays and dictionaries

Getting values resembles Python syntax. Shai is 0 indexed so arrays start at index 0:

```
my_array = [1, 2, 3]

my_first_number = my_array[0] // note 0 means the first item here

my_dict = {
    "hello": "world"
    "hi": "mom"
}

my_first_number = my_dict["hello"] // "world"

```

## Functions

You can declare blocks of functionality called Functions:

```

// declare a function
// it has a name so you can reference it
// the "name" between the brackets are called the paramters, these are your input into the function
say_hi (name) {
    // the code in this block will get run
    print "Hi" name
}

// this is how you run (called "calling") a function...notice similarities with printing? print is also a function!
say_hi "Reader :)"

```

The language currently has 2 builtin functions for you: `print`, which you already know about, displays text to the screen. There is also a `len` function to get the length of something you can loop over:

```

my_values =  [1, 2, 3]

my_values_len = len my_values

print my_values_len

```

