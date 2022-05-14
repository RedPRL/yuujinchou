# 👹 Yuujinchou (友人帳): Name Modifiers

_Yuujinchou_ is an OCaml package of name modifiers. Please consult the [API documentation](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou) for more details.

## Components

- [Yuujinchou.Trie](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou/Trie): efficient namespaces for hierarchical names
- [Yuujinchou.Language](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou/Language): name modifiers and selectors
- [Yuujinchou.Modifier](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou/Modifier): the engine running modifiers
- [Yuujinchou.Selector](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou/Selector): the engine running selectors
- [Yuujinchou.Scope](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou/Scope): nested scopes

## Example Code

- [A tiny interpreter using Yuujinchou.Scope](test/Example.ml)

## Installation

You need a version of OCaml that supports algebraic effects.
Currently, it means OCaml >= 5.0.0, OCaml 4.12+domains, or OCaml 4.12+domains+effects.
The package is available in the OPAM repository:
```
opam install yuujinchou
```

You could also pin the latest version in development:
```
opam pin https://github.com/RedPRL/yuujinchou.git
```
