# Kaleidoscope Compiler

This is a personal project trying to implement a kaleidoscope compiler in
rust. This is by no means intended for anything other than educational
purposes for myself.

## How It's Intended To Work

I aim to have a number of components. These components will their own
individual rust packages.

### Lexer

Breaks a string into tokens.

**Status:** In Progress.

### Parser

Converts the tokens provided by the _Lexer_ into a Abstract Syntax Tree
(AST).

**Status:** Not Started.

### Codegen

Converts the AST provided by the _Parser_ into LLVM Intermediate
Representation (IR).

**Status:** Not Started.

### Runtime

Not sure if this is needed yet. But if needed it would provide some
externally callable functions that our output program can be linked to in
order to use.

**Status:** Not Started.

### Compiler

Binds all other rust packages into an executable that someone can run. In addition to this is will handle things like:

- Reading files.
- Link to potential runtime.
- Outputting the final executable.

**Status:** Not Started.

## Useful Links

I try to follow [this](https://llvm.org/docs/tutorial/) tutorial the best I
can. It is written in C++ but the basic principals are the same.
