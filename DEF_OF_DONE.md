# Definition of Done

This document outlines what constitutes "done". It gives the requirements for the minimum viable v1.

Shai-Lang's first version will be considered "done" when the language:

- has a basic interpreter that allows for a "Read-Eval-Print" Loop (or REPL for short). This allows declaring variables, functions, expressions and the other language features.
- tooling for running a Shai file.
- can be built from source using Rust tooling. 
- has adequate documentation which includes API documentation and an overview of the language syntax and usage.
- has appropriate CLI tooling to invoke the Shai REPL and Shai scripts.

## Stretch goals

Some additional goals that will improve the language would be:

- a Language Server implementation to allow for code completion, diagnostics and other expected features of a modern development experience. The quality for this would aim to match the Lua language server due to the languages similarities.
- a tutorial building some simple helper scripts for usage.
- shell integration and being able to source and execute bash.
- hosting on remote repositories such as apt or homebrew.
