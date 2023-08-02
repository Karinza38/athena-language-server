# Athena Language Server

A language server implementation for the Athena proof language.

## Installing

You need to have both cargo and npm installed.

Run

```bash
cargo xtask install
```

## Implementation Status

The primary components relevant to providing a good editing experience are:

- Parser
- Semantic analysis

### Parser

#### Dependent features

- Rich syntax highlighting          : Solid
- Syntax error feedback on typing   : Solid
- Everything that depends on semantic analysis

#### Status

The parser is feature complete and exhaustively tested. All valid syntax should be correctly parsed, and error messages should be useful in the case
of a syntax error. If you find a case where this isn't true, it's a bug!

### Semantic Analysis

This is a pretty large umbrella, but the two largest pieces are name resolution and
inferring type information.

#### Dependent features

- Go to definition              : Working but incomplete
- Full semantic highlighting    : Working but incomplete
- Completion                    : Not yet implemented

#### Status

Semantic analysis is still in its early stages. Currently in the process of defining a more useful HIR (high-level intermediate representation).
There is an existing HIR, but it is cumbersome to work with and the lowering process is entangled with name resolution. This means that name
resolution has to work off of the AST, which isn't ideal as the AST faithfully represents every piece of syntax of the language. That's good
when you need to work with syntax, but annoying when you don't care about minor syntactic differences (as in semantic analysis)!

The new HIR is progressing, but Athena is not a small language which means there are a lot of cases to cover when lowering from AST to HIR.
Once the HIR is in place, semantic analysis should go much more smoothly going forward.
