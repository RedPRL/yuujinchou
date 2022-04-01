(* The following shim does nothing for OCaml >= 5, but is needed for OCaml < 5. *)
open Eff.StdlibShim

open Yuujinchou
open Bwd

(* A tiny language demonstrating some power of the Scope module. *)
type pattern_cmd = Print
type decl =
  | Decl of Trie.path * int
  | ShadowingDecl of Trie.path * int (* supressing the shadowing warning *)
  | Import of int Trie.t * pattern_cmd Pattern.t (* importing a trie after applying the pattern *)
  | PrintVisible
  | Export of Trie.path
  | Section of Trie.path * decl list
type program = decl list

(* Specialzed Scope module with Data.t *)
module S = Scope.Make (struct type data = int type hook = pattern_cmd end)
module A = S.Act

(* Convert a backward path into a string for printing. *)
let string_of_bwd_path =
  function
  | Emp -> "(root)"
  | path -> String.concat "." (BwdLabels.to_list path)

(* Handle effects from running the patterns. *)
let handle_pattern_effects f =
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | A.BindingNotFound path -> Option.some @@
            fun (k : (a, _) continuation) ->
            Format.printf "[Warning] Could not find any data within the subtree at %s.@."
              (string_of_bwd_path path);
            continue k ()
          | A.Shadowing (path, old_data, new_data) -> Option.some @@
            fun (k : (a, _) continuation) ->
            if Int.equal old_data new_data then
              continue k old_data
            else begin
              Format.printf "[Warning] Data %i assigned at %s was shadowed by data %i.@."
                old_data (string_of_bwd_path path) new_data;
              continue k new_data
            end
          | A.Hook (Print, path, trie) -> Option.some @@
            fun (k : (a, _) continuation) ->
            Format.printf "@[<v 2>[Info] Got the following bindings at the prefix %s:@;"
              (string_of_bwd_path path);
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
          | A.Shadowing (_path, _old_data, new_data) -> Option.some @@
            fun (k : (a, _) continuation) -> continue k new_data
          | _ -> None }

(* The interpreter *)
let rec interpret_decl =
  function
  | Decl (p, x) ->
    S.include_singleton (p, x)
  | ShadowingDecl (p, x) ->
    silence_shadowing @@ fun () ->
    S.include_singleton (p, x)
  | Import (t, pat) ->
    let t = A.run pat t in
    S.import_subtree ([], t)
  | PrintVisible ->
    S.run_on_visible (Pattern.hook Print)
  | Export p ->
    S.export_visible (Pattern.only p)
  | Section (p, sec) ->
    S.section p @@ fun () -> List.iter interpret_decl sec
let interpret prog =
  handle_pattern_effects @@ fun () ->
  S.run @@ fun () ->
  List.iter interpret_decl prog

(* Some code in action *)
let () = interpret [
    Decl (["x"], 1);
    PrintVisible;
    Decl (["x"], 2);
    PrintVisible;
    ShadowingDecl (["x"], 10);
    PrintVisible;
    Import (Trie.of_seq (List.to_seq [["y"], 20]), Pattern.renaming [] ["z"]);
    PrintVisible;
    Export ["z"; "y"];
    Section (["w"], [
        Decl (["a"], 100);
        PrintVisible;
        Export ["x"];
      ]);
    PrintVisible;
  ]
