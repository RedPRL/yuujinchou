open Bwd
open BwdNotation
open Yuujinchou
open Language

let trie (type a) (elem : a Alcotest.testable) : (a, unit) Trie.t Alcotest.testable =
  (module struct
    type t = (a, unit) Trie.t
    let pp fmt t = Alcotest.(pp @@ list @@ pair (list string) (pair elem unit)) fmt (List.of_seq @@ Trie.to_seq t)
    let equal = Trie.equal (Alcotest.equal elem) Alcotest.(equal unit)
  end)

let bwd (type a) (elem : a Alcotest.testable) : a bwd Alcotest.testable =
  (module struct
    type t = a bwd
    let pp fmt b = Alcotest.(pp @@ list elem) fmt (BwdLabels.to_list b)
    let equal = BwdLabels.equal ~eq:(Alcotest.equal elem)
  end)

type data = N of int | U of string bwd * data * data

let data : data Alcotest.testable =
  (module struct
    type t = data
    let rec pp fmt =
      function
      | N i -> Format.pp_print_int fmt i
      | U (rp, d1, d2) -> Format.fprintf fmt "U(%a,%a,%a)" Alcotest.(pp @@ bwd string) rp pp d1 pp d2
    let equal = (=)
  end)

type empty = |
module M = Modifier.Make (struct type nonrec data = data type tag = unit type hook = empty type context = empty end)

exception WrappedBindingNotFound of Trie.bwd_path
let wrap f =
  M.run f
    { not_found = (fun ?context:_ prefix -> raise @@ WrappedBindingNotFound prefix);
      shadow = (fun ?context:_ path (x, ()) (y, ()) -> U (path, x, y), ());
      hook = (fun ?context:_ _ -> function _ -> .) }

let wrap_error f = fun () -> wrap @@ fun () -> ignore (f ())

let of_list l = Trie.(Untagged.tag () @@ Untagged.of_seq (List.to_seq l))

let test_none_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> M.modify none (of_list [["x"], N 10]))

let test_none_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> M.modify none (of_list [[], N 10]))

let test_none_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.modify none Trie.empty)

let test_any_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (wrap @@ fun () -> M.modify any (of_list [["x"], N 10]))

let test_any_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [[], N 10])
    (wrap @@ fun () -> M.modify any (of_list [[], N 10]))

let test_any_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.modify any Trie.empty)

let test_any_4 () =
  let t = of_list [[], N 10] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.modify any t)

let test_only_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (wrap @@ fun () -> M.modify (only ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_only_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "x"], N 20])
    (wrap @@ fun () -> M.modify (only ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_only_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x" #< "y"))
    (wrap_error @@ fun () -> M.modify (only ["x"; "y"]) Trie.empty)

let test_only_4 () =
  let t = of_list [["x"], N 10] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.modify (only ["x"]) t)

let test_except_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 20])
    (wrap @@ fun () -> M.modify (except ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_except_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 30])
    (wrap @@ fun () -> M.modify (except ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_except_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x" #< "y"))
    (wrap_error @@ fun () -> M.modify (except ["x"; "y"]) Trie.empty)

let test_in_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "z"], N 10; ["y"], N 20])
    (wrap @@ fun () -> M.modify (in_ ["x"] (renaming [] ["z"])) (of_list [["x"], N 10; ["y"], N 20]))

let test_in_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30])
    (wrap @@ fun () -> M.modify (in_ ["x"] (renaming ["x"] ["w"])) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_in_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x"))
    (wrap_error @@ fun () -> M.modify (in_ ["x"] any) Trie.empty)

let test_in_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.modify (in_ ["x"] (renaming ["x"] ["x"])) t)

let test_renaming_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"], N 10; ["y"], N 20])
    (wrap @@ fun () -> M.modify (renaming ["x"] ["z"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_renaming_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"; "y"], N 10; ["z"; "x"], N 20; ["y"], N 30])
    (wrap @@ fun () -> M.modify (renaming ["x"] ["z"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_renaming_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x"))
    (wrap_error @@ fun () -> M.modify (renaming ["x"] ["z"]) Trie.empty)

let test_renaming_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.modify (renaming ["x"] ["x"]) t)

let test_renaming_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.modify (renaming ["x"; "y"] ["x"; "y"]) t)

let test_seq_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["w"], N 10; ["y"], N 20])
    (wrap @@ fun () -> M.modify (seq [renaming ["x"] ["z"]; renaming ["z"] ["w"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_2 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.modify (seq [none; any]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_seq_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.modify (seq [none; none]) Trie.empty)

let test_seq_4 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x"))
    (wrap_error @@ fun () -> M.modify (seq [renaming ["x"] ["z"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.modify (seq [seq []; seq []]) t)

let test_union_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U ((Emp #< "x"), N 10, N 10); ["y"], N 20])
    (wrap @@ fun () -> M.modify (union [only ["x"]; except ["x"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U ((Emp #< "x"), N 10, N 10); ["y"], N 20])
    (wrap @@ fun () -> M.modify (in_ ["x"] (union [only []; only []])) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_3 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> M.modify (union []) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.modify (union [seq []]) t)

(* FIXME: design new test cases for hooks *)

let () =
  let open Alcotest in
  run "Modifier" [
    "none", [
      test_case "none" `Quick test_none_1;
      test_case "none" `Quick test_none_2;
      test_case "none" `Quick test_none_3;
    ];
    "any", [
      test_case "any" `Quick test_any_1;
      test_case "any" `Quick test_any_2;
      test_case "any" `Quick test_any_3;
      test_case "any" `Quick test_any_4;
    ];
    "only", [
      test_case "only" `Quick test_only_1;
      test_case "only" `Quick test_only_2;
      test_case "only" `Quick test_only_3;
      test_case "only" `Quick test_only_4;
    ];
    "except", [
      test_case "except" `Quick test_except_1;
      test_case "except" `Quick test_except_2;
      test_case "except" `Quick test_except_3;
    ];
    "in_", [
      test_case "in_" `Quick test_in_1;
      test_case "in_" `Quick test_in_2;
      test_case "in_" `Quick test_in_3;
      test_case "in_" `Quick test_in_4;
    ];
    "renaming", [
      test_case "renaming" `Quick test_renaming_1;
      test_case "renaming" `Quick test_renaming_2;
      test_case "renaming" `Quick test_renaming_3;
      test_case "renaming" `Quick test_renaming_4;
      test_case "renaming" `Quick test_renaming_5;
    ];
    "seq", [
      test_case "seq" `Quick test_seq_1;
      test_case "seq" `Quick test_seq_2;
      test_case "seq" `Quick test_seq_3;
      test_case "seq" `Quick test_seq_4;
      test_case "seq" `Quick test_seq_5;
    ];
    "union", [
      test_case "union" `Quick test_union_1;
      test_case "union" `Quick test_union_2;
      test_case "union" `Quick test_union_3;
      test_case "union" `Quick test_union_4;
    ];
  ]
