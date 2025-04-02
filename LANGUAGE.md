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

Declaration:

`// C `

where

C = comment text

OR

/*
   C
*/
C = comment text

Examples:
`// This is a comment`

```
/*
    This is a
    multiline comment
*/
```

## Data Types

- `char`
- `string`
- `int`
- `bool`
- `float`
- `[T]`(arrays)
    - where T is the item stored in the array
- `{K, V}`(dictionaries)
    - where K is the Key type and V is the Value type

## Variable assignment

Declaration:

`x = y`

where

x = variable name
T = variable data type
y = variable value

OR

x = y

where

x = variable name
y = variable value

> [!NOTE]
> Type is inferred automatically

Examples:
`x : int = 5`
    OR
`x = 5`


`y : string = "hello"`
    OR
`y = "hello"`

`b : bool = true`
    OR
`b = true`

## Variable Constants

Declaration:

`const x = y`
OR
`const x T = y`

where

x = variable name
T = type
y = value

Examples:
const x : int = 5 // immutable

## Functions

Declaration:

fx (y T1, z T2, ... T...) -> RT
{ ... }

where

fx = function name
y, z and ... = arguments
T1, T2 and T... = argument types
RT = return type
{ ... } = function body

OR

fx (y, z, ...) -> RT { ... }

where

fx = function name
y, z and ... = arguments (types inferred)
RT = return type

OR

fx (y, z, ...) { ... }

where

fx = function name
y, z and ... = arguments (types inferred)

> [!NOTE]
> Return type is inferred

Examples:

```
calculateArea (l int, w int) -> int {
    return l * w
}
```

```
calculateArea (x, y) -> int {
    return x * y
}
```

```
concatString (s1, s2) -> string {
    return s1 + s2
}
```

```
concatString (s1, s2) {
    return s1 + s2 // Return type is string
}
```

```
concatStringAndInt (s1, i1) {
    return s1 + i1 // Return type is string
}
```

## Array Literals

Declaration:

`a : [T] = [v1, v2, v3]`

where

T = array type
a = array name
\[v1, v2, v3] = array initialization values

Examples:
```
values : [string] = ["one", "two", "three"]
```
## Function calls

Declaration:

`x ...`

where

x = function name
... = arguments specified

Examples:
> Without variable
`calculateArea 9 10`
    OR
> With inferred type
`area = calculateArea 9 10`
    OR
> With explicit type
`area : int = calculateArea 9 10`


## Property/Method Access

Declaration:

`e.p`

where

e = expression
p = property on expression

Examples:

```
"hello".chars // char[]
```
```
"5".parseInt // 5
```

## Imports

Declaration:

`include x`

where

x = file name

OR

include x as y

where

x = file name
y = alias

Examples:

```
include "foo"

// usage
x = someFunctionFromFoo()
```
```
include "bar" as baz

x = baz.someFunctionFromFoo()
```

## If else expressions

Declaration:

`if x { ... }`

where

x = condition to enter block if evaluated to true

Examples:
```
if true {
    //always go in here
}
```
```
i = 5
if i > 3 {
    print i
}
```

```
i = 5
if i > 3 {
    print i
} else {
    print i + 1
}
```
## For Loops

Declaration:

```
for x in y {
    ...
}

```

where

x = scoped variable
y = range

Examples:
```
for i in 0..10{
    print i
}
```
```
names = ["Jordan", "Jules", "Sarah"]
for name in names {
    print name
}
```

## While loop

Declaration:

```
while x {

}
```

where

x is loop continuation condition that evaluates to true or false.

> [!NOTE]
> If x is ommitted, loop will continue until a break statement is hit

```
while {
    print "hello"
    // infinite loop
}
```
```
// print hello 10 times
i = 0
while i < 10 {
    print "hello"
    i+=1
}
```
```
// print hello 3 times
i = 0
while i < 10 {
    print "hello"
    if i / 2 == 1 {
        break
    }
    i+=1
}
```
