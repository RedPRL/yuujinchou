open Yuujinchou
open Pattern

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  let module M = struct
    type t = a Trie.t
    let pp = Trie.pp (Alcotest.pp elem)
    let equal = Trie.equal (Alcotest.equal elem)
  end in
  (module M)

let of_list l = Trie.of_seq (fun _ _ -> failwith "conflicting keys") (List.to_seq l)

let error : Action.error Alcotest.testable =
  let module M = struct
    type t = Action.error
    let pp = Action.pp_error
    let equal e1 e2 =
      match e1, e2 with
      | Action.BindingNotFound p1,
        Action.BindingNotFound p2 -> p1 = p2
  end in
  (module M)

let run_result elem = Alcotest.result (trie elem) error

let test_none_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run (+) none (of_list [["x"], 10]))

let test_none_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run (+) none (of_list [[], 10]))

let test_none_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) none Trie.empty)

let test_any_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run (+) any (of_list [["x"], 10]))

let test_any_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [[], 10]))
    (Action.run (+) any (of_list [[], 10]))

let test_any_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) any Trie.empty)

let test_wildcard_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run (+) wildcard (of_list [["x"], 10]))

let test_wildcard_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) wildcard (of_list [[], 10]))

let test_wildcard_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) wildcard Trie.empty)

let test_root_1 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) root (of_list [["x"], 10]))

let test_root_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [[], 10]))
    (Action.run (+) root (of_list [[], 10]))

let test_root_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) root Trie.empty)

let test_only_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run (+) (only ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_only_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (only ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20]))

let test_only_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (only ["x"]) Trie.empty)

let test_only_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run (+) (only_subtree ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_only_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "y"], 10; ["x"; "x"], 20]))
    (Action.run (+) (only_subtree ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_only_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (only_subtree ["x"]) Trie.empty)

let test_except_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 20]))
    (Action.run (+) (except ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_except_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (except ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20]))

let test_except_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (except ["x"]) Trie.empty)

let test_except_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 20]))
    (Action.run (+) (except_subtree ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_except_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 30]))
    (Action.run (+) (except_subtree ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_except_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (except_subtree ["x"]) Trie.empty)

let test_on_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "z"], 10; ["y"], 20]))
    (Action.run (+) (on_subtree ["x"] (renaming [] ["z"])) (of_list [["x"], 10; ["y"], 20]))

let test_on_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30]))
    (Action.run (+) (on_subtree ["x"] (renaming ["x"] ["w"])) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_on_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (on_subtree ["x"] any) Trie.empty)

let test_renaming_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"], 10; ["y"], 20]))
    (Action.run (+) (renaming ["x"] ["z"]) (of_list [["x"], 10; ["y"], 20]))

let test_renaming_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (renaming ["x"] ["z"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_renaming_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (renaming ["x"] ["z"]) Trie.empty)

let test_renaming_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"], 10; ["y"], 20]))
    (Action.run (+) (renaming_subtree ["x"] ["z"]) (of_list [["x"], 10; ["y"], 20]))

let test_renaming_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"; "y"], 10; ["z"; "x"], 20; ["y"], 30]))
    (Action.run (+) (renaming_subtree ["x"] ["z"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_renaming_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (renaming_subtree ["x"] ["z"]) Trie.empty)

let test_seq_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["w"], 10; ["y"], 20]))
    (Action.run (+) (seq [renaming ["x"] ["z"]; renaming ["z"] ["w"]]) (of_list [["x"], 10; ["y"], 20]))

let test_seq_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) (seq [none; any]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_seq_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound []))
    (Action.run (+) (seq [none; none]) Trie.empty)

let test_seq_4 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (BindingNotFound ["x"]))
    (Action.run (+) (seq [renaming ["x"] ["z"]; only ["x"]]) (of_list [["x"], 10; ["y"], 20]))

let test_union_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10; ["y"], 20]))
    (Action.run (+) (union [only ["x"]; except ["x"]]) (of_list [["x"], 10; ["y"], 20]))

let test_union_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run (+) (union []) (of_list [["x"], 10; ["y"], 20]))

let test_filter_map_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 110]))
    (Action.run (+) (filter_map (fun d -> if d > 20 then Some (d + 80) else None))
       (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

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
    ];
    "wildcard", [
      test_case "wildcard" `Quick test_wildcard_1;
      test_case "wildcard" `Quick test_wildcard_2;
      test_case "wildcard" `Quick test_wildcard_3;
    ];
    "root", [
      test_case "root" `Quick test_root_1;
      test_case "root" `Quick test_root_2;
      test_case "root" `Quick test_root_3;
    ];
    "only", [
      test_case "only" `Quick test_only_1;
      test_case "only" `Quick test_only_2;
      test_case "only" `Quick test_only_3;
    ];
    "only_subtree", [
      test_case "only_subtree" `Quick test_only_subtree_1;
      test_case "only_subtree" `Quick test_only_subtree_2;
      test_case "only_subtree" `Quick test_only_subtree_3;
    ];
    "except", [
      test_case "except" `Quick test_except_1;
      test_case "except" `Quick test_except_2;
      test_case "except" `Quick test_except_3;
    ];
    "except_subtree", [
      test_case "except_subtree" `Quick test_except_subtree_1;
      test_case "except_subtree" `Quick test_except_subtree_2;
      test_case "except_subtree" `Quick test_except_subtree_3;
    ];
    "on_subtree", [
      test_case "on_subtree" `Quick test_on_subtree_1;
      test_case "on_subtree" `Quick test_on_subtree_2;
      test_case "on_subtree" `Quick test_on_subtree_3;
    ];
    "renaming", [
      test_case "renaming" `Quick test_renaming_1;
      test_case "renaming" `Quick test_renaming_2;
      test_case "renaming" `Quick test_renaming_3;
    ];
    "renaming_subtree", [
      test_case "renaming_subtree" `Quick test_renaming_subtree_1;
      test_case "renaming_subtree" `Quick test_renaming_subtree_2;
      test_case "renaming_subtree" `Quick test_renaming_subtree_3;
    ];
    "seq", [
      test_case "seq" `Quick test_seq_1;
      test_case "seq" `Quick test_seq_2;
      test_case "seq" `Quick test_seq_3;
      test_case "seq" `Quick test_seq_4;
    ];
    "union", [
      test_case "union" `Quick test_union_1;
      test_case "union" `Quick test_union_2;
    ];
    "filter_map", [
      test_case "filter_map" `Quick test_filter_map_1;
    ];
  ]
