# Shai-lang Syntax + Grammar

## General overview

### What constitutes true?
- true
- conditions that evaluate to true - e.g. 5 == 5

### Type design:
Language is dynamically typed, but types can be specified.
If these don't conform, this would result in a runtime error.

### Syntax goals:
Simple structure that resembles other popular languages.
Cut out unncessary syntax.

### Features:
Type extensions
Higher order functions + anonymous functions

### Inspirations:
Bash    - function calls and signature general syntax
Nushell - idea of scripts being more structured
C       - syntax of arrays, constants
Go      - type specification in function signatures + variables, no semicolons
Python  - dynamic types
Rust    - return type syntax
... and common language syntax, data types

## Comments

Declaration:

// C 

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

## "Primitive" Data Types

- `char`
- `string`
- `int`
- `bool`
- `float`
- `T[]`(arrays)
    - where T is the item stored in the array
- `K, V{}`(dictionaries)
    - where K is the Key type and V is the Value type

## Variable assignment

Declaration: 

x : T = y

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

x : const T = y

where

x = variable name
T = type
y = value

Examples: 
x : const int = 5 // immutable

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

> [!NOTE]
> Whitespace between function name and argument list

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

T[] a = \[v1, v2, v3]\

where 

T = array type
a = array name
\[v1, v2, v3] = array initialization values

Examples: 
```
string[] values = ["one", "two", "three"]
```
## Function calls

Declaration: 

x(args) 

where 

x = function name
args = arguments specified

Examples:
> Without variable
`calculateArea 9, 10`
    OR
> With inferred type
`area = calculateArea 9, 10`
    OR  
> With explicit type 
`area : int = calculateArea 9, 10`


## Property/Method Access

Declaration:

e.p

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

include x

where

x = file name

OR 

include x as y

where

x = file name
y = alias

Examples:

```
include foo
```
```
include bar as baz
```

## If condtions
Declaration: 

if x { ... }

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
## For Loops

Declaration:

for x in y {
    ...
}

where 

x = scoped variable
y = range

Examples: 
```
for i in range(0, 10){
    print i
}
```
```
names = ["Jordan", "Jules", "Sarah"]
for name in names{
    print name
}
```

## While loop 
Declaration:

while x { 
    
}

where 

x is loop continuation condition
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