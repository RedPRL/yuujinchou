open YuujinchouNew

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  let module M = struct
    type t = a Trie.t
    let pp = Trie.pp (Alcotest.pp elem)
    let equal = Trie.equal (Alcotest.equal elem)
  end in
  (module M)

let of_list l = Trie.of_seq (fun _ _ -> failwith "conflicting keys") (List.to_seq l)

let test_empty () =
  Alcotest.(check @@ trie int) "same trie" (of_list []) Trie.empty

let test_is_empty_empty () =
  Alcotest.(check bool) "empty" true (Trie.is_empty Trie.empty)

let test_is_empty_root () =
  Alcotest.(check bool) "not empty" false (Trie.is_empty @@ of_list [[], [1]])

let test_mk_root_none () =
  Alcotest.(check @@ trie int) "same trie" Trie.empty (Trie.mk_root None)

let test_mk_root_some () =
  Alcotest.(check @@ trie int) "same trie" (of_list [[], 10]) (Trie.mk_root (Some 10))

let test_prefix_1 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"], 10; ["x"; "y"; "z"], 100])
    (Trie.prefix ["x"] (of_list [[], 10; ["y"; "z"], 100]))

let test_prefix_2 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x1"; "x2"; "y"], 10; ["x1"; "x2"; "z"], 20])
    (Trie.prefix ["x1"; "x2"] (of_list [["y"], 10; ["z"], 20]))

let test_equal_1 () =
  Alcotest.(check bool) "true"
    true
    (Trie.equal Alcotest.(equal int) (of_list [["x"; "y"], 100]) (of_list [["x"; "y"], 100]))

let test_equal_2 () =
  Alcotest.(check bool) "false"
    false
    (Trie.equal Alcotest.(equal int) (of_list [["x"; "y"], 100]) (of_list [["x"; "z"], 100]))

let test_union () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 10+160; [], 20; ["x"], 40])
    (Trie.union (+) (of_list [["x"; "y"], 10; [], 20]) (of_list [["x"], 40; ["x"; "y"], 160]))

let test_union_subtree () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 10; [], 20; ["x"], 40+80; ["x"; "x"; "y"], 1600])
    (Trie.union_subtree (+) (of_list [["x"; "y"], 10; [], 20; ["x"], 40]) (["x"], of_list [[], 80; ["x"; "y"], 1600]))

let test_detach_subtree () =
  Alcotest.(check @@ pair (trie int) (trie int)) "same trie"
    (of_list [["y"], 10], of_list [[], 20])
    (Trie.detach_subtree ["x"] (of_list [["x"; "y"], 10; [], 20]))

let test_detach_singleton_1 () =
  Alcotest.(check @@ pair (option int) (trie int)) "same trie"
    (None, of_list [["x"; "y"], 10; [], 20])
    (Trie.detach_singleton ["x"] (of_list [["x"; "y"], 10; [], 20]))

let test_detach_singleton_2 () =
  Alcotest.(check @@ pair (option int) (trie int)) "same trie"
    (Some 20, of_list [["x"; "y"], 10])
    (Trie.detach_singleton [] (of_list [["x"; "y"], 10; [], 20]))

let test_filter_map_endo () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [[], 30])
    (Trie.filter_map_endo (fun x -> if x > 10 then Some 30 else None) (of_list [["x"; "y"], 10; [], 20]))

let () =
  let open Alcotest in
  run "Trie" [
    "empty", [
      test_case "empty" `Quick test_empty;
    ];
    "is_empty", [
      test_case "is_empty" `Quick test_is_empty_empty;
      test_case "is_empty" `Quick test_is_empty_root;
    ];
    "mk_root", [
      test_case "mk_root" `Quick test_mk_root_none;
      test_case "mk_root" `Quick test_mk_root_some;
    ];
    "prefix", [
      test_case "prefix" `Quick test_prefix_1;
      test_case "prefix" `Quick test_prefix_2;
    ];
    "equal", [
      test_case "equal" `Quick test_equal_1;
      test_case "equal" `Quick test_equal_2;
    ];
    "union", [
      test_case "union" `Quick test_union;
    ];
    "union_subtree", [
      test_case "union_subtree" `Quick test_union_subtree;
    ];
    "detach_subtree", [
      test_case "detach_subtree" `Quick test_detach_subtree;
    ];
    "detach_singleton", [
      test_case "detach_singleton" `Quick test_detach_singleton_1;
      test_case "detach_singleton" `Quick test_detach_singleton_2;
    ];
    "filter_map_endo", [
      test_case "filter_map_endo" `Quick test_filter_map_endo;
    ];
  ]
