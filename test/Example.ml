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
  | Import of int Trie.Untagged.t * modifier_cmd Language.t
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

(* Handle scoping effects *)
let handler : _ Scope.handler =
  let pp_path fmt =
    function
    | Emp -> Format.pp_print_string fmt "(root)"
    | path -> Format.pp_print_string fmt @@ String.concat "." (Bwd.to_list path)
  in
  let pp_context fmt =
    function
    | Some `Visible -> Format.pp_print_string fmt " in the visible namespace"
    | Some `Export -> Format.pp_print_string fmt " in the export namespace"
    | None -> ()
  in
  let pp_item fmt =
    function
    | (x, `Imported) -> Format.fprintf fmt "%i (imported)" x
    | (x, `Local) -> Format.fprintf fmt "%i (local)" x
  in
  { not_found =
      (fun ?context prefix ->
         Format.printf
           "[Warning] Could not find any data within the subtree at %a%a.@."
           pp_path prefix pp_context context);
    shadow =
      (fun ?context path x y ->
         Format.printf
           "[Warning] Data %a assigned at %a was shadowed by data %a%a.@."
           pp_item x
           pp_path path
           pp_item y
           pp_context context;
         y);
    hook =
      (fun ?context prefix hook input ->
         match hook with
         | Print ->
           Format.printf "@[<v 2>[Info] Got the following bindings at %a%a:@;"
             pp_path prefix pp_context context;
           Trie.iter
             (fun path x ->
                Format.printf "%a => %a@;" pp_path path pp_item x)
             input;
           Format.printf "@]@.";
           input)}

(* Mute the [shadow] effects. *)
let silence_shadow f = S.run_modifier f {S.perform with shadow = fun ?context:_ _ _ y -> y}

(* The interpreter *)
let rec interpret_decl : decl -> unit =
  function
  | Decl (p, x) ->
    S.include_singleton ~context_visible:`Visible ~context_export:`Export (p, (x, `Local))
  | ShadowingDecl (p, x) ->
    silence_shadow @@ fun () ->
    S.include_singleton (p, (x, `Local))
  | Import (t, m) ->
    let t = S.modify m (Trie.Untagged.tag `Imported t) in
    S.import_subtree ([], t)
  | PrintVisible ->
    S.modify_visible (Language.hook Print)
  | Export p ->
    S.export_visible (Language.only p)
  | Section (p, sec) ->
    S.section p @@ fun () -> List.iter interpret_decl sec

let interpret (prog : program) =
  S.run (fun () -> List.iter interpret_decl prog) handler

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
