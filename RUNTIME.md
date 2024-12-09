# Runtime notes

## Initial thoughts

- We should look at Python's runtime or OCaml's UTOP
- We need a Read-Eval-Print loop (REPL) 
- 


## REPL

### READ 

We need to read all valid sections of code.

e.g. in function declarations

```shai
x () { // read from this line
    my_val = 5
    my_other_val = 6
    return my_val + my_other_val
} // to this line
```
We want to read this whole block in, not just a single line.


```shai
x = 5
```

But here we just want to read a line.

In other words, we need to figure out when we're "finished" reading input, and then to treat entering a newline differently.

## How to treat newlines

If we are top level, and there are no bodies being opened:

```shai
x = 5
```
then in this context, newline will run the command.

If we recognise in a line that we have an open brace with no closing brace, e.g. in a function declaration

```shai
x () {
    ...
```
or expression assignment

```shai
x = { ... 
```

or inline expressions

```shai
someFunc "hello" {...
```

then this will require more lines to be completed until we find a matching closing brace. 

## Eval step

We have a parsed syntax tree at this point.

Example simplified tree:

```
Function:
    name: x 
    args: numOne, numTwo
    return type: int 
    body: 
        Body:
            Return:
                lhs: numOne 
                rhs: numTwo
                Operation: add
```

We need to create a *runtime* to store declarations and be able to lookup references.

Call stack for functions, to figure out local scopes of parameters and variables.

### Thought process

We start on the global scope.

Variables and functions get stored here in some form.

Variables can be easy:

`x = 5`, we would just have a key 'x' set to 5

But this can get more complicated already:

`x = y + z` where y and z are variables referenced above.

When it comes to running that code, we would need to do a lookup for y and z in our scope table.

Then we perform our operation on them.

---

Further complications with functions

Each function gets it's own hash table and access to the global hash table.

```shai
x (numOne int, numTwo int) -> int { 
    return numOne + numTwo
}
```

We would store our function x in the enclosing scope hash table.

Then when we call it, we execute that behaviour.

In fact, we could just store the syntax tree, and then we evaluate it when the function is called.

> [!QUESTION]
> We need to figure out how to store a body. Maybe something to give a type? Or we could use our expression tree and then evaluate at runtime?

#### What does evaluating mean here though? 

It means traversing the tree.
If any references are made, like variables declarations, storing those.
If any operations occur, it means executing them, using the references.

This actually reminds me of the fetch decode execute cycle of cpus.

We fetch the next instruction. We decode the references. We execute an operation. And then we go to the next instruction.

Therefore, we can use this cycle to model how we should do this.


## Hook Design

The runtime will expose a hook `run` to allow for different execution contexts. These will be the REPL runtime, and the standard file runtime.

This will take in an application `ScopeState` that the caller will manage, and the most recent `code_blob` representing a complete line or block of code.

Examples of a `code_blob` are: 

```
// single line
x = 5
```


```
//multi line
x () { 
    return 46
}
```

```
//multi line
if true { 
    ...
} else {
    ...
}
```
