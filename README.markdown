# Yuujinchou: Name Pattern Combinators

_Yuujinchou_ is an OCaml package of name patterns for implementing import statements. Please consult the [generated documentation](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou) for more details.

## How to Use It

```ocaml
open Yuujinchou

module Data =
struct
  type t = int
  let equal = Int.equal
  let shadow ~rev_path:_ _x y = y
  let compare = Int.compare
end

(** An environment is a mapping from paths to data. *)
type env = Data.t Trie.t

(* Specialzed Action module with Data.t *)
type empty = |
module A = Action.Make (struct type data = Data.t type hook = empty end)

(** [remap pattern env] uses the [pattern] to massage
    the environment [env]. *)
let remap pattern env =
  let open Effect.Deep in
  let string_of_path = function [] -> "(root)" | path -> String.concat "." path in
  try_with (A.run pattern) env
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | A.BindingNotFound rev_path -> Option.some @@
            fun (k : (a, _) continuation) ->
            Format.printf "[Warning]@ Could not find any data within the subtree at %s.@."
              (string_of_path @@ List.rev rev_path);
            continue k ()
          | A.Shadowing (rev_path, old_data, new_data) -> Option.some @@
            fun (k : (a, _) continuation) ->
            if Data.equal old_data new_data then
              continue k old_data
            else begin
              Format.printf "[Warning]@ Data %i assigned at %s was shadowed by data %i.@."
                old_data (string_of_path @@ List.rev rev_path) new_data;
              continue k new_data
            end
          | A.Hook _ -> .
          | _ -> None }

(** [import env pattern imported] imports the environment
    [imported] massaged by [pattern] into [env]. *)
let import env pattern imported =
  Trie.union Data.shadow env @@ remap pattern imported

module DataSet = Set.Make (Data)

(** [select env pattern] returns the set of matched data. *)
let select env pattern =
  DataSet.of_seq @@ Trie.to_seq_values @@ remap pattern env
```

## Installation

You need OCaml 4.08.0 or newer. The package is available on the OPAM repository:
```
opam install yuujinchou
```

You could also check out the source repository and install the latest version in development:
```
git clone https://github.com/RedPRL/yuujinchou.git
opam install ./yuujinchou
```
