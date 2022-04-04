# Yuujinchou: Name Pattern Combinators

_Yuujinchou_ is an OCaml package of name patterns for implementing import statements. Please consult the [API documentation](https://redprl.org/yuujinchou/yuujinchou/Yuujinchou) for more details.

## How to Use It

<!-- This part should be in sync with test/Example.ml and src/Yuujinchou.mli -->
```ocaml
(* The following shim does nothing for OCaml >= 5, but is needed for OCaml < 5. *)
open Algaeff.StdlibShim

open Yuujinchou
open Bwd

(* A tiny language demonstrating some power of the Scope module. *)
type pattern_cmd = Print
type decl =
  (* declaration *)
  | Decl of Trie.path * int
  (* declaration, but supressing the shadowing warning *)
  | ShadowingDecl of Trie.path * int
  (* importing a trie after applying the pattern *)
  | Import of int Trie.t * pattern_cmd Pattern.t
  (* printing out all visible bindings *)
  | PrintVisible
  (* exporting a binding *)
  | Export of Trie.path
  (* section *)
  | Section of Trie.path * decl list
type program = decl list

(* Specialzed Scope module with Data.t *)
module S = Scope.Make (struct type data = int type hook = pattern_cmd end)

(* New source label for imported namespaces *)
type S.Act.source += Imported

(* Convert a backward path into a string for printing. *)
let string_of_bwd_path =
  function
  | Emp -> "(root)"
  | path -> String.concat "." (BwdLabels.to_list path)

(* Handle effects from running the patterns. *)
let handle_pattern_effects f =
  let open Effect.Deep in
  let string_of_source =
    function
    | Some S.Visible -> " in the visible namespace"
    | Some S.Export -> " in the export namespace"
    | Some Imported -> " in the imported namespace"
    | _ -> " in an unknown namespace"
  in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | S.Act.BindingNotFound (src, path) -> Option.some @@
            fun (k : (a, _) continuation) ->
            Format.printf "[Warning] Could not find any data within the subtree at %s%s.@."
              (string_of_bwd_path path) (string_of_source src);
            continue k ()
          | S.Act.Shadowing (src, path, old_data, new_data) -> Option.some @@
            fun (k : (a, _) continuation) ->
            begin
              Format.printf "[Warning] Data %i assigned at %s was shadowed by data %i%s.@."
                old_data (string_of_bwd_path path) new_data (string_of_source src);
              continue k new_data
            end
          | S.Act.Hook (src, path, Print, trie) -> Option.some @@
            fun (k : (a, _) continuation) ->
            Format.printf "@[<v 2>[Info] Got the following bindings at %s%s:@;"
              (string_of_bwd_path path) (string_of_source src);
            Trie.iteri
              (fun ~path data ->
                 Format.printf "%s => %i@;" (string_of_bwd_path path) data)
              trie;
            Format.printf "@]@.";
            continue k trie
          | _ -> None }

(* Mute the shadowing effects. *)
let silence_shadowing f =
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | S.Act.Shadowing (_src, _path, _old_data, new_data) -> Option.some @@
            fun (k : (a, _) continuation) -> continue k new_data
          | _ -> None }

(* The interpreter *)
let rec interpret_decl : decl -> unit =
  function
  | Decl (p, x) ->
    S.include_singleton (p, x)
  | ShadowingDecl (p, x) ->
    silence_shadowing @@ fun () ->
    S.include_singleton (p, x)
  | Import (t, pat) ->
    let t = S.Act.run ~source:Imported pat t in
    S.import_subtree ([], t)
  | PrintVisible ->
    S.run_on_visible (Pattern.hook Print)
  | Export p ->
    S.export_visible (Pattern.only p)
  | Section (p, sec) ->
    S.section p @@ fun () -> List.iter interpret_decl sec

let interpret (prog : program) =
  handle_pattern_effects @@ fun () ->
  S.run @@ fun () ->
  List.iter interpret_decl prog
```

## Installation

You need a version of OCaml that supports algebraic effects.
Currently, it means OCaml >= 5.0.0, OCaml 4.12+domains, or OCaml 4.12+domains+effects
The package is available in the OPAM repository:
```
opam install yuujinchou
```

You could also pin the latest version in development:
```
opam pin https://github.com/RedPRL/yuujinchou.git
```
