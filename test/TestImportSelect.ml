(** {1 Example Code} *)

open Yuujinchou

module Data =
struct
  type t = int
  let equal n1 n2 = n1 = n2
  let merge x y =
    if equal x y then x
    else failwith "Inconsistent data assigned to the same path."
  let shadow _x y = y
  let compare : t -> t -> int = compare
end

(** An environment is a mapping from paths to data. *)
type env = Data.t Trie.t

(** [remap pattern env] uses the [pattern] to massage
    the environment [env]. *)
let remap pattern env =
  match Action.run Data.merge pattern env with
  | Ok env -> env
  | Error (Action.BindingNotFound path) ->
    failwith ("Expected item not found within the subtree rooted at #root." ^ String.concat "." path ^ ".")

(** [import env pattern imported] imports the environment
    [imported] massaged by [pattern] into [env]. *)
let import env pattern imported =
  Trie.union Data.shadow env @@ remap pattern imported

module DataSet = Set.Make (Data)

(** [select env pattern] returns the set of matched data. *)
let select env pattern =
  DataSet.of_seq @@ Seq.map snd @@ Trie.to_seq @@ remap pattern env

(** {1 Testing} *)

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  let module M = struct
    type t = a Trie.t
    let pp = Trie.pp (Alcotest.pp elem)
    let equal = Trie.equal (Alcotest.equal elem)
  end in
  (module M)

let data_set : DataSet.t Alcotest.testable =
  let module M = struct
    type t = DataSet.t
    let pp fmt s = Alcotest.(pp (list int) fmt (List.of_seq (DataSet.to_seq s)))
    let equal = DataSet.equal
  end in
  (module M)

let trie_of_list l = Trie.of_seq (fun _ _ -> failwith "conflicting keys") (List.to_seq l)

let set_of_list l = DataSet.of_seq (List.to_seq l)

let test_import_1 () =
  Alcotest.(check @@ trie int) "ok"
    (trie_of_list [["x"], 20])
    (import (trie_of_list [["x"], 10]) Pattern.any (trie_of_list [["x"], 20]))

let test_import_2 () =
  Alcotest.(check @@ trie int) "ok"
    (trie_of_list [["x"], 10])
    (import (trie_of_list [["x"], 10]) Pattern.none (trie_of_list [["x"], 20]))

let test_select_1 () =
  Alcotest.(check data_set) "ok"
    (set_of_list [10])
    (select (trie_of_list [["x"], 10]) Pattern.any)

let test_select_2 () =
  Alcotest.(check data_set) "ok"
    (set_of_list [])
    (select (trie_of_list [["x"], 10]) Pattern.none)

let () =
  let open Alcotest in
  run "TestImportSelect" [
    "import", [
      test_case "import" `Quick test_import_1;
      test_case "import" `Quick test_import_2;
    ];
    "select", [
      test_case "select" `Quick test_select_1;
      test_case "select" `Quick test_select_2;
    ];
  ]
