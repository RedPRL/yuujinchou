# [3.0.0](/compare/2.0.0...3.0.0) (2022-06-03)

Version 3.0.0 is a complete redesign and rewrite of the library, with the following notable changes:

1. Algebraic effects are used everywhere.
2. A new module `Scope` was introduced for lexical scoping.
3. Tries were augmented with tags to support constant-time retagging.
4. Renaming now shadows conflicting names in the target subtree instead of merging with them. See `Language.renaming`.
5. There is a design document under `docs/`.

The old client code that works with Yuujinchou 2.0.0 will stop working with the new version.


# [2.0.0](https://github.com/RedPRL/yuujinchou/compare/1.0.0...2.0.0) (2022-03-07)


### Bug Fixes

* **action:** pass rev_prefix to the final union ([#58](https://github.com/RedPRL/yuujinchou/issues/58)) ([000df38](https://github.com/RedPRL/yuujinchou/commit/000df384e4cb75773ed25b185f7c2a3f86acfdaa))


### Code Refactoring

* **trie:** drop almost all requirements of physical equality ([#54](https://github.com/RedPRL/yuujinchou/issues/54)) ([262e1be](https://github.com/RedPRL/yuujinchou/commit/262e1be09fed1005e5ddf87cc9f3d7de6810d79c))


### Features

* **action:** monadic union ([#57](https://github.com/RedPRL/yuujinchou/issues/57)) ([6a95ace](https://github.com/RedPRL/yuujinchou/commit/6a95aceed8469cd55a380edd70ebaa2fe5e143ca))
* **trie:** monadic updates with Stdlib.result ([#55](https://github.com/RedPRL/yuujinchou/issues/55)) ([b2e783b](https://github.com/RedPRL/yuujinchou/commit/b2e783ba465865e0479a99ba9430e29b3956cc0d))


### BREAKING CHANGES

* **action:** run and run_with_hooks now take a monadic merger
* **trie:** drop all *_endo functions and physical equality requirements
