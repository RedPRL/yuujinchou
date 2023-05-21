# [5.0.0](https://github.com/RedPRL/yuujinchou/compare/4.0.0...5.0.0) (2022-05-21)

### Features

- **Language:** add the abbreviation `id = seq []` ([#106](https://github.com/RedPRL/yuujinchou/issues/106)) ([12ffa87](https://github.com/RedPRL/yuujinchou/commit/12ffa8707f5331ef7e2956aa0b711483c65dfed8))
- **Scope:** added `import_singleton` ([#105](https://github.com/RedPRL/yuujinchou/issues/105)) ([c12f9c5](https://github.com/RedPRL/yuujinchou/commit/c12f9c5c6f36403c31832626e940bd310dc74578))
- **Scope:** many operations now take modifiers ([#108](https://github.com/RedPRL/yuujinchou/issues/108)) ([8c1ded9](https://github.com/RedPRL/yuujinchou/commit/8c1ded926de6b9944b5ca861bcf06990feebcf32))

### BREAKING CHANGES

All breaking changes come with type changes. That is, if OCaml is still happy with your code that worked with 4.0.0, your code _will_ work with 5.0.0.

- `Language.any` is renamed to `Language.any` ([#107](https://github.com/RedPRL/yuujinchou/issues/107)) ([313f616](https://github.com/RedPRL/yuujinchou/commit/313f6168072a35af4fafe6e9e02555b6b434850e))
- `Scope.S.modify` is removed; instead, many operations now take optional modifiers ([#108](https://github.com/RedPRL/yuujinchou/issues/108)) ([8c1ded9](https://github.com/RedPRL/yuujinchou/commit/8c1ded926de6b9944b5ca861bcf06990feebcf32))
- If you have been passing contexts to operations in `Scope.S`, argument names might have been changed. Previously, some context arguments were named `context`, but now they are uniformly named as `conetext_visible`, `context_export`, or `context_modifier` depending on their purposes.

# [4.0.0](https://github.com/RedPRL/yuujinchou/compare/3.1.0...4.0.0) (2022-12-16)

### Features

- register printers for yuujinchou effects ([#102](https://github.com/RedPRL/yuujinchou/issues/102)) ([3b89480](https://github.com/RedPRL/yuujinchou/commit/3b89480eaa250ba9a2b7de6b3086342d2cfb1a0d))
- **Scope:** parametrize `Scope.Make` by a modifier ([#98](https://github.com/RedPRL/yuujinchou/issues/98)) ([4a2f3ef](https://github.com/RedPRL/yuujinchou/commit/4a2f3efb6ee565a18ac4ebef536e61214978b893))
- **Trie:** add `Trie.untag` ([4671c83](https://github.com/RedPRL/yuujinchou/commit/4671c83b24c83a7df136ae62c80f2ecca9d76b22))
- **Trie:** add type alias `untagged` ([031ac7a](https://github.com/RedPRL/yuujinchou/commit/031ac7aa1db3ac0ba1e0ba568fcc40fee3d2e2ae))

### BREAKING CHANGES

- break the functor `Run` into `Run` and `TryWith` ([#97](https://github.com/RedPRL/yuujinchou/issues/97)) ([3ac136c](https://github.com/RedPRL/yuujinchou/commit/3ac136ccb283e9f0795e6a13e351a12a01a56f92))
- use labelled arguments instead of functors ([#99](https://github.com/RedPRL/yuujinchou/issues/99)) ([256b5e7](https://github.com/RedPRL/yuujinchou/commit/256b5e726575d0912c28a1165ce49d9f6851dfda))

# [3.1.0](https://github.com/RedPRL/yuujinchou/compare/3.0.0...3.1.0) (2022-07-11)

### BREAKING CHANGES

- Use modules instead of records to pass effect handlers ([#94](https://github.com/RedPRL/yuujinchou/issues/94))

# [3.0.0](https://github.com/RedPRL/yuujinchou/compare/2.0.0...3.0.0) (2022-06-03)

### BREAKING CHANGES

Version 3.0.0 is a complete redesign and rewrite of the library, with the following notable changes:

1. Algebraic effects are used everywhere.
2. A new module `Scope` was introduced for lexical scoping.
3. Tries were augmented with tags to support constant-time retagging.
4. Renaming now shadows conflicting names in the target subtree instead of merging with them. See `Language.renaming`.
5. There is a design document under `docs/`.

The old client code that works with yuujinchou 2.0.0 will stop working with the new version.

# [2.0.0](https://github.com/RedPRL/yuujinchou/compare/1.0.0...2.0.0) (2022-03-07)

### Bug Fixes

- **action:** pass rev_prefix to the final union ([#58](https://github.com/RedPRL/yuujinchou/issues/58)) ([000df38](https://github.com/RedPRL/yuujinchou/commit/000df384e4cb75773ed25b185f7c2a3f86acfdaa))

### Code Refactoring

- **trie:** drop almost all requirements of physical equality ([#54](https://github.com/RedPRL/yuujinchou/issues/54)) ([262e1be](https://github.com/RedPRL/yuujinchou/commit/262e1be09fed1005e5ddf87cc9f3d7de6810d79c))

### Features

- **action:** monadic union ([#57](https://github.com/RedPRL/yuujinchou/issues/57)) ([6a95ace](https://github.com/RedPRL/yuujinchou/commit/6a95aceed8469cd55a380edd70ebaa2fe5e143ca))
- **trie:** monadic updates with Stdlib.result ([#55](https://github.com/RedPRL/yuujinchou/issues/55)) ([b2e783b](https://github.com/RedPRL/yuujinchou/commit/b2e783ba465865e0479a99ba9430e29b3956cc0d))

### BREAKING CHANGES

- **action:** run and run_with_hooks now take a monadic merger
- **trie:** drop all \*\_endo functions and physical equality requirements
