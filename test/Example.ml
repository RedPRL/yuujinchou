(* The following shim does nothing for OCaml >= 5, but is needed for OCaml < 5. *)
open Yuujinchou
open Bwd

(* A tiny language demonstrating some power of the Scope module. *)
type modifier_cmd = Print
type decl =
  (* declaration *)
  | Decl of Trie.path * int
  (* declaration, but supressing the shadowing warning *)
  | ShadowingDecl of Trie.path * int
  (* importing a trie after applying the modifier *)
  | Import of int Trie.untagged * modifier_cmd Language.t
  (* printing out all visible bindings *)
  | PrintVisible
  (* exporting a binding *)
  | Export of Trie.path
  (* section *)
  | Section of Trie.path * decl list
type program = decl list

(* Specialzed Scope module with Data.t *)
module S = Scope.Make (struct
    type data = int
    type tag = [`Imported | `Local]
    type hook = modifier_cmd
    type context = [`Visible | `Export]
  end)

(* Convert a backward path into a string for printing. *)
let string_of_bwd_path =
  function
  | Emp -> "(root)"
  | path -> String.concat "." (BwdLabels.to_list path)

(* Handle effects from running the modifiers. *)
let handle_modifier_effects f =
  let open Effect.Deep in
  let string_of_context =
    function
    | Some `Visible -> " in the visible namespace"
    | Some `Export -> " in the export namespace"
    | None -> ""
  in
  let string_of_tag =
    function
    | `Imported -> " (imported)"
    | `Local -> " (local)"
  in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | S.Mod.BindingNotFound {context; prefix} -> Option.some @@
            fun (k : (a, _) continuation) ->
            Format.printf "[Warning] Could not find any data within the subtree at %s%s.@."
              (string_of_bwd_path prefix) (string_of_context context);
            continue k ()
          | S.Mod.Shadowing {context; path; former; latter} -> Option.some @@
            fun (k : (a, _) continuation) ->
            begin
              Format.printf "[Warning] Data %i assigned at %s was shadowed by data %i%s.@."
                (fst former) (string_of_bwd_path path) (fst latter) (string_of_context context);
              continue k latter
            end
          | S.Mod.Hook {context; prefix; hook = Print; input} -> Option.some @@
            fun (k : (a, _) continuation) ->
            Format.printf "@[<v 2>[Info] Got the following bindings at %s%s:@;"
              (string_of_bwd_path prefix) (string_of_context context);
            Trie.iter
              (fun path (data, tag) ->
                 Format.printf "%s => %i%s@;" (string_of_bwd_path path) data (string_of_tag tag))
              input;
            Format.printf "@]@.";
            continue k input
          | _ -> None }

(* Mute the shadowing effects. *)
let silence_shadowing f =
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | S.Mod.Shadowing {latter; _} -> Option.some @@
            fun (k : (a, _) continuation) -> continue k latter
          | _ -> None }

(* The interpreter *)
let rec interpret_decl : decl -> unit =
  function
  | Decl (p, x) ->
    S.include_singleton ~context_visible:`Visible ~context_export:`Export (p, (x, `Local))
  | ShadowingDecl (p, x) ->
    silence_shadowing @@ fun () ->
    S.include_singleton (p, (x, `Local))
  | Import (t, m) ->
    let t = S.Mod.exec m (Trie.tag `Imported t) in
    S.import_subtree ([], t)
  | PrintVisible ->
    S.modify_visible (Language.hook Print)
  | Export p ->
    S.export_visible (Language.only p)
  | Section (p, sec) ->
    S.section p @@ fun () -> List.iter interpret_decl sec

let interpret (prog : program) =
  handle_modifier_effects @@ fun () ->
  S.run (fun () -> List.iter interpret_decl prog)

(* Some code in action *)
let () = interpret [
    Decl (["x"], 1);
    PrintVisible;
    Decl (["x"], 2);
    PrintVisible;
    ShadowingDecl (["x"], 10);
    PrintVisible;
    Import (Trie.Untagged.of_seq (List.to_seq [["y"], 20]), Language.renaming [] ["z"]);
    PrintVisible;
    Export ["z"; "y"];
    Section (["w"], [
        Decl (["a"], 100);
        PrintVisible;
        Export ["x"];
        Export ["x"];
      ]);
    PrintVisible;
  ]
