open Bwd
open BwdNotation
open Yuujinchou
open Language

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  (module struct
    type t = a Trie.t
    let pp fmt t = Alcotest.(pp @@ list @@ pair (list string) elem) fmt (List.of_seq @@ Trie.to_seq t)
    let equal = Trie.equal (Alcotest.equal elem)
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
module M = Modifier.Make (struct type nonrec data = data type hook = empty end)

exception WrappedBindingNotFound of Trie.bwd_path
let wrap f =
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | M.BindingNotFound {prefix; _} -> Option.some @@
            fun (k : (a, _) continuation) ->
            discontinue k @@ WrappedBindingNotFound prefix
          | M.Shadowing {path; former; latter; _} -> Option.some @@
            fun (k : (a, _) continuation) ->
            continue k @@ U (path, former, latter)
          | M.Hook _ -> .
          | _ -> None
    }

let wrap_error f = fun () -> wrap @@ fun () -> ignore (f ())

let of_list l = Trie.of_seq (List.to_seq l)

let test_none_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> M.exec none (of_list [["x"], N 10]))

let test_none_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> M.exec none (of_list [[], N 10]))

let test_none_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.exec none Trie.empty)

let test_any_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (wrap @@ fun () -> M.exec any (of_list [["x"], N 10]))

let test_any_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [[], N 10])
    (wrap @@ fun () -> M.exec any (of_list [[], N 10]))

let test_any_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.exec any Trie.empty)

let test_any_4 () =
  let t = of_list [[], N 10] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.exec any t)

let test_only_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (wrap @@ fun () -> M.exec (only ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_only_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "x"], N 20])
    (wrap @@ fun () -> M.exec (only ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_only_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x" #< "y"))
    (wrap_error @@ fun () -> M.exec (only ["x"; "y"]) Trie.empty)

let test_only_4 () =
  let t = of_list [["x"], N 10] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.exec (only ["x"]) t)

let test_except_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 20])
    (wrap @@ fun () -> M.exec (except ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_except_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 30])
    (wrap @@ fun () -> M.exec (except ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_except_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x" #< "y"))
    (wrap_error @@ fun () -> M.exec (except ["x"; "y"]) Trie.empty)

let test_in_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "z"], N 10; ["y"], N 20])
    (wrap @@ fun () -> M.exec (in_ ["x"] (renaming [] ["z"])) (of_list [["x"], N 10; ["y"], N 20]))

let test_in_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30])
    (wrap @@ fun () -> M.exec (in_ ["x"] (renaming ["x"] ["w"])) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_in_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x"))
    (wrap_error @@ fun () -> M.exec (in_ ["x"] any) Trie.empty)

let test_in_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.exec (in_ ["x"] (renaming ["x"] ["x"])) t)

let test_renaming_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"], N 10; ["y"], N 20])
    (wrap @@ fun () -> M.exec (renaming ["x"] ["z"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_renaming_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"; "y"], N 10; ["z"; "x"], N 20; ["y"], N 30])
    (wrap @@ fun () -> M.exec (renaming ["x"] ["z"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_renaming_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x"))
    (wrap_error @@ fun () -> M.exec (renaming ["x"] ["z"]) Trie.empty)

let test_renaming_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.exec (renaming ["x"] ["x"]) t)

let test_renaming_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.exec (renaming ["x"; "y"] ["x"; "y"]) t)

let test_seq_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["w"], N 10; ["y"], N 20])
    (wrap @@ fun () -> M.exec (seq [renaming ["x"] ["z"]; renaming ["z"] ["w"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_2 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.exec (seq [none; any]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_seq_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound Emp)
    (wrap_error @@ fun () -> M.exec (seq [none; none]) Trie.empty)

let test_seq_4 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound (Emp #< "x"))
    (wrap_error @@ fun () -> M.exec (seq [renaming ["x"] ["z"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.exec (seq [seq []; seq []]) t)

let test_union_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U ((Emp #< "x"), N 10, N 10); ["y"], N 20])
    (wrap @@ fun () -> M.exec (union [only ["x"]; except ["x"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U ((Emp #< "x"), N 10, N 10); ["y"], N 20])
    (wrap @@ fun () -> M.exec (in_ ["x"] (union [only []; only []])) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_3 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> M.exec (union []) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> M.exec (union [seq []]) t)

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
