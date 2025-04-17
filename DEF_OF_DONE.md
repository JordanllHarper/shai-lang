# Definition of Done

This document outlines what constitutes "done". It gives the requirements for the minimum viable v1.

Shai-Lang's first version will be considered "done" when the language:

- [x] The Interpreter can be a standalone binary or built from source using Rust tooling.
- [x] Shai scripts can be run using the CLI tooling.
- [x] Language features:
    - [x] Print to standard output
    - [x] Perform basic math operations with operator precedence
    - [x] Store values in variables
    - [x] Conditionally branch given a condition is true or false
    - [x] Both while and for loops
    - [x] Dictionary and array collections
    - [x] Functions with contained stack values
- [ ] Has adequate documentation which includes API documentation and an overview of the language syntax and usage.

## Stretch goals

Some additional goals that will improve the language would be:

- a "Read-Eval-Print" Loop (or REPL for short). This allows declaring variables, functions, expressions and the other language features.
- a Language Server implementation to allow for code completion, diagnostics and other expected features of a modern development experience. The quality for this would aim to match the Lua language server due to the languages similarities.
- a tutorial building some simple helper scripts for usage.
- shell integration and being able to source and execute bash.
- hosting on remote repositories such as apt or homebrew.
