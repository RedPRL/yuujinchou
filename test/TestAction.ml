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


exception WrappedBindingNotFound of path
let wrap f x =
  let open Effect.Deep in
  ignore @@ try_with f x
    { effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Action.BindingNotFound path -> Option.some @@
            fun (k : (a, _) continuation) ->
            discontinue k @@ WrappedBindingNotFound path
          | _ -> None
    }

let merger ~rev_path x y = U (rev_path, x, y)

let of_list l =
  Trie.of_seq
    (fun ~rev_path _ _ -> failwith @@ "conflicting keys at " ^ String.concat "." @@ List.rev rev_path)
    (List.to_seq l)

let test_none_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (Action.run ~merger none (of_list [["x"], N 10]))

let test_none_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (Action.run ~merger none (of_list [[], N 10]))

let test_none_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap @@ fun () -> Action.run ~merger none Trie.empty)

let test_any_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (Action.run ~merger any (of_list [["x"], N 10]))

let test_any_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [[], N 10])
    (Action.run ~merger any (of_list [[], N 10]))

let test_any_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap @@ fun () -> Action.run ~merger any Trie.empty)

let test_any_4 () =
  let t = of_list [[], N 10] in
  Alcotest.(check @@ trie data) "true"
    t
    (Action.run ~merger any t)

let test_only_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], N 10])
    (Action.run ~merger (only ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_only_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "x"], N 20])
    (Action.run ~merger (only ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_only_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap @@ fun () -> Action.run ~merger (only ["x"]) Trie.empty)

let test_only_4 () =
  let t = of_list [["x"], N 10] in
  Alcotest.(check @@ trie data) "ok"
    t
    (Action.run ~merger (only ["x"]) t)

let test_except_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 20])
    (Action.run ~merger (except ["x"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_except_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["y"], N 30])
    (Action.run ~merger (except ["x"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_except_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap @@ fun () -> Action.run ~merger (except ["x"]) Trie.empty)

let test_in_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "z"], N 10; ["y"], N 20])
    (Action.run ~merger (in_ ["x"] (renaming [] ["z"])) (of_list [["x"], N 10; ["y"], N 20]))

let test_in_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30])
    (Action.run ~merger (in_ ["x"] (renaming ["x"] ["w"])) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_in_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap @@ fun () -> Action.run ~merger (in_ ["x"] any) Trie.empty)

let test_in_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (Action.run ~merger (in_ ["x"] (renaming ["x"] ["x"])) t)

let test_renaming_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"], N 10; ["y"], N 20])
    (Action.run ~merger (renaming ["x"] ["z"]) (of_list [["x"], N 10; ["y"], N 20]))

let test_renaming_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["z"; "y"], N 10; ["z"; "x"], N 20; ["y"], N 30])
    (Action.run ~merger (renaming ["x"] ["z"]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_renaming_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap @@ fun () -> Action.run ~merger (renaming ["x"] ["z"]) Trie.empty)

let test_renaming_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (Action.run ~merger (renaming ["x"] ["x"]) t)

let test_renaming_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (Action.run ~merger (renaming ["x"; "y"] ["x"; "y"]) t)

let test_seq_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["w"], N 10; ["y"], N 20])
    (Action.run ~merger (seq [renaming ["x"] ["z"]; renaming ["z"] ["w"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_2 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap @@ fun () -> Action.run ~merger (seq [none; any]) (of_list [["x"; "y"], N 10; ["x"; "x"], N 20; ["y"], N 30]))

let test_seq_3 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound [])
    (wrap @@ fun () -> Action.run ~merger (seq [none; none]) Trie.empty)

let test_seq_4 () =
  Alcotest.check_raises "error"
    (WrappedBindingNotFound ["x"])
    (wrap @@ fun () -> Action.run ~merger (seq [renaming ["x"] ["z"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_seq_5 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (Action.run ~merger (seq [seq []; seq []]) t)

let test_union_1 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U (["x"], N 10, N 10); ["y"], N 20])
    (Action.run ~merger (union [only ["x"]; except ["x"]; only ["x"]]) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_2 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [["x"], U (["x"], N 10, N 10); ["y"], N 20])
    (Action.run ~merger (in_ ["x"] (union [only []; only []])) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_3 () =
  Alcotest.(check @@ trie data) "ok"
    (of_list [])
    (Action.run ~merger (union []) (of_list [["x"], N 10; ["y"], N 20]))

let test_union_4 () =
  let t = of_list [["x"; "y"], N 10; ["x"; "w"], N 20; ["y"], N 30] in
  Alcotest.(check @@ trie data) "ok"
    t
    (Action.run ~merger (union [seq []]) t)

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
