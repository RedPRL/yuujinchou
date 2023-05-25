open Yuujinchou

(* A tiny language demonstrating some power of the Scope module. *)
type decl =
  | Decl of Trie.path * int
  | Import of int Trie.untagged * unit Language.t
  | Export of Trie.path
  | Section of Trie.path * decl list
  | PrintInt of Trie.path

type program = decl list

module P =
struct
  type data = int
  type tag = unit

  type hook = unit (* for modifier hooks; unused here *)
  type context = unit (* for advanced printing and reporting; unused here *)
end

(* Specialized Scope module *)
module S = Scope.Make (P)

let pp_path fmt =
  function
  | [] -> Format.pp_print_string fmt "(root)"
  | path -> Format.pp_print_string fmt @@ String.concat "." path

(* The interpreter *)
let rec interpret_decl : decl -> unit =
  function
  | Decl (p, x) ->
    S.include_singleton (p, (x, ()))
  | Import (t, m) ->
    let t = Trie.retag () t in
    S.import_subtree ~modifier:m ([], t)
  | Export p ->
    S.export_visible (Language.only p)
  | Section (p, sec) ->
    S.section p @@ fun () -> interpret_section sec
  | PrintInt p ->
    match S.resolve p with
    | None -> Format.eprintf "Unbound variable %a@." pp_path p
    | Some (i, _) -> Format.printf "%a = %i@." pp_path p i

and interpret_section section =
  List.iter interpret_decl section

let interpret (prog : program) =
  S.run @@ fun () -> interpret_section prog

let () = interpret [
    Decl (["x"], 1);
    Decl (["x"], 2);
    Import (Trie.Untagged.of_seq (List.to_seq [["y"], 20]), Language.renaming [] ["z"]);
    Export ["z"; "y"];
    Section (["w"], [
        Decl (["a"], 100);
        Export ["x"];
      ]);
    PrintInt ["z"; "y"];
    PrintInt ["w"; "a"];
    PrintInt ["w"; "x"];
  ]
