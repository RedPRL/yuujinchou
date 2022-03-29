open Yuujinchou
open Pattern

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  (module struct
    type t = a Trie.t
    let pp fmt t = Alcotest.(pp @@ list @@ pair (list string) elem) fmt (List.of_seq @@ Trie.to_seq t)
    let equal = Trie.equal (Alcotest.equal elem)
  end)

type data = N of int | U of string list * data * data

let data : data Alcotest.testable =
  (module struct
    type t = data
    let rec pp fmt =
      function
      | N i -> Format.pp_print_int fmt i
      | U (rp, d1, d2) -> Format.fprintf fmt "U(%a,%a,%a)" Alcotest.(pp @@ list string) rp pp d1 pp d2
    let equal = (=)
  end)

type empty = |
module A = Action.Make (struct type nonrec data = data type hook = empty end)

exception WrappedBindingNotFound of path
let wrap f =
  let open Effect.Deep in
  try_with f ()
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | A.BindingNotFound rev_path -> Option.some @@
            fun (k : (a, _) continuation) ->
            discontinue k @@ WrappedBindingNotFound rev_path
          | A.Shadowing (rev_path, x, y) -> Option.some @@
            fun (k : (a, _) continuation) ->
            continue k @@ U (rev_path, x, y)
          | _ -> None
    }

let wrap_error f = fun () -> wrap @@ fun () -> ignore (f ())

let of_list l =
  Trie.of_seq
    (fun ~rev_path _ _ -> failwith @@ "conflicting keys at " ^ String.concat "." @@ List.rev rev_path)
    (List.to_seq l)

let test_none_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> A.run none (of_list [["x"], N 10]))

let test_none_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> A.run none (of_list [[], N 10]))

let test_none_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap_error @@ fun () -> A.run none Trie.empty)

let test_any_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (wrap @@ fun () -> A.run any (of_list [["x"], N 10]))

let test_any_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [[], N 10])
    (wrap @@ fun () -> A.run any (of_list [[], N 10]))

let test_any_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap_error @@ fun () -> A.run any Trie.empty)

let test_any_4 () =
  let t = of_list [[], N 10] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> A.run any t)

let test_only_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (wrap @@ fun () -> A.run (only ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_only_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "x"], N 20])
    (wrap @@ fun () -> A.run (only ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_only_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["y"; "x"])
    (wrap_error @@ fun () -> A.run (only ["x"; "y"]) Trie.empty)

let test_only_4 () =
  let t = of_list [["x"], N 10] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> A.run (only ["x"]) t)

let test_except_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 20])
    (wrap @@ fun () -> A.run (except ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_except_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 30])
    (wrap @@ fun () -> A.run (except ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_except_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["y"; "x"])
    (wrap_error @@ fun () -> A.run (except ["x"; "y"]) Trie.empty)

let test_in_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "z"], N 10; ["y"], N 20])
    (wrap @@ fun () -> A.run (in_ ["x"] (renaming [] ["z"])) (of_list [["x"], N 10; ["y"], N 20]))

let test_in_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30])
    (wrap @@ fun () -> A.run (in_ ["x"] (renaming ["x"] ["w"])) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_in_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap_error @@ fun () -> A.run (in_ ["x"] any) Trie.empty)

let test_in_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> A.run (in_ ["x"] (renaming ["x"] ["x"])) t)

let test_renaming_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"], N 10; ["y"], N 20])
    (wrap @@ fun () -> A.run (renaming ["x"] ["z"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_renaming_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"; "y"], N 10; ["z"; "x"], N 20; ["y"], N 30])
    (wrap @@ fun () -> A.run (renaming ["x"] ["z"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_renaming_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap_error @@ fun () -> A.run (renaming ["x"] ["z"]) Trie.empty)

let test_renaming_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> A.run (renaming ["x"] ["x"]) t)

let test_renaming_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> A.run (renaming ["x"; "y"] ["x"; "y"]) t)

let test_seq_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["w"], N 10; ["y"], N 20])
    (wrap @@ fun () -> A.run (seq [renaming ["x"] ["z"]; renaming ["z"] ["w"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_2 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap_error @@ fun () -> A.run (seq [none; any]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_seq_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap_error @@ fun () -> A.run (seq [none; none]) Trie.empty)

let test_seq_4 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap_error @@ fun () -> A.run (seq [renaming ["x"] ["z"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> A.run (seq [seq []; seq []]) t)

let test_union_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U (["x"], N 10, N 10); ["y"], N 20])
    (wrap @@ fun () -> A.run (union [only ["x"]; except ["x"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U (["x"], N 10, N 10); ["y"], N 20])
    (wrap @@ fun () -> A.run (in_ ["x"] (union [only []; only []])) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_3 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (wrap @@ fun () -> A.run (union []) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (wrap @@ fun () -> A.run (union [seq []]) t)

(* FIXME: design new test cases for hooks *)

let () =
  let open Alcotest in
  run "Action" [
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
