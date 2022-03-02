open Yuujinchou
open Pattern

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  let module M = struct
    type t = a Trie.t
    let pp fmt t = Alcotest.(pp @@ list @@ pair (list string) elem) fmt (List.of_seq @@ Trie.to_seq t)
    let equal = Trie.equal (Alcotest.equal elem)
  end in
  (module M)

let cantor ~rev_path:_ x y = if x == y then x else (x + y) * (x + y + 1) / 2 + y

let of_list l =
  Trie.of_seq
    (fun ~rev_path _ _ -> failwith @@ "conflicting keys at " ^ String.concat "." @@ List.rev rev_path)
    (List.to_seq l)

let error : [`BindingNotFound of Pattern.path] Alcotest.testable =
  let module M = struct
    type t = [`BindingNotFound of Pattern.path]
    let pp fmt (`BindingNotFound path) = Format.fprintf fmt "Binding not found: %a" Action.pp_path path
    let equal (`BindingNotFound p1) (`BindingNotFound p2) = p1 = p2
  end in
  (module M)

let run_result elem = Alcotest.result (trie elem) error

let test_none_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run ~union:cantor none (of_list [["x"], 10]))

let test_none_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run ~union:cantor none (of_list [[], 10]))

let test_none_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor none Trie.empty)

let test_any_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run ~union:cantor any (of_list [["x"], 10]))

let test_any_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [[], 10]))
    (Action.run ~union:cantor any (of_list [[], 10]))

let test_any_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor any Trie.empty)

let test_any_4 () =
  let t = of_list [[], 10] in
  Alcotest.(check bool) "true"
    true
    (Trie.equal Int.equal t @@ Result.get_ok (Action.run ~union:cantor any t))

let test_only_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run ~union:cantor (only ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_only_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "y"], 10; ["x"; "x"], 20]))
    (Action.run ~union:cantor (only ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_only_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (only ["x"]) Trie.empty)

let test_only_4 () =
  let t = of_list [["x"], 10] in
  Alcotest.(check @@ run_result int) "ok"
    (Ok t)
    (Action.run ~union:cantor (only ["x"]) t)

let test_except_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 20]))
    (Action.run ~union:cantor (except ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_except_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 30]))
    (Action.run ~union:cantor (except ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_except_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (except ["x"]) Trie.empty)

let test_in_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "z"], 10; ["y"], 20]))
    (Action.run ~union:cantor (in_ ["x"] (renaming [] ["z"])) (of_list [["x"], 10; ["y"], 20]))

let test_in_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30]))
    (Action.run ~union:cantor (in_ ["x"] (renaming ["x"] ["w"])) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_in_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (in_ ["x"] any) Trie.empty)

let test_in_4 () =
  let t = of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30] in
  Alcotest.(check @@ run_result int) "ok"
    (Ok t)
    (Action.run ~union:cantor (in_ ["x"] (renaming ["x"] ["x"])) t)

let test_renaming_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"], 10; ["y"], 20]))
    (Action.run ~union:cantor (renaming ["x"] ["z"]) (of_list [["x"], 10; ["y"], 20]))

let test_renaming_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"; "y"], 10; ["z"; "x"], 20; ["y"], 30]))
    (Action.run ~union:cantor (renaming ["x"] ["z"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_renaming_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (renaming ["x"] ["z"]) Trie.empty)

let test_renaming_4 () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check @@ run_result int) "ok"
    (Ok t)
    (Action.run ~union:cantor (renaming ["x"] ["x"]) t)

let test_renaming_5 () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check @@ run_result int) "ok"
    (Ok t)
    (Action.run ~union:cantor (renaming ["x"; "y"] ["x"; "y"]) t)

let test_seq_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["w"], 10; ["y"], 20]))
    (Action.run ~union:cantor (seq [renaming ["x"] ["z"]; renaming ["z"] ["w"]]) (of_list [["x"], 10; ["y"], 20]))

let test_seq_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor (seq [none; any]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_seq_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor (seq [none; none]) Trie.empty)

let test_seq_4 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (seq [renaming ["x"] ["z"]; only ["x"]]) (of_list [["x"], 10; ["y"], 20]))

let test_seq_5 () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check @@ run_result int) "ok"
    (Ok t)
    (Action.run ~union:cantor (seq [seq []; seq []]) t)

let test_union_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10; ["y"], 20]))
    (Action.run ~union:cantor (union [only ["x"]; except ["x"]]) (of_list [["x"], 10; ["y"], 20]))

let test_union_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run ~union:cantor (union []) (of_list [["x"], 10; ["y"], 20]))

let test_union_3 () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check @@ run_result int) "ok"
    (Ok t)
    (Action.run ~union:cantor (union [seq []]) t)

let test_filter_map_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 110]))
    (Action.run_with_hooks
       ~hooks:(fun () ~rev_prefix t ->
           Result.ok @@ Trie.filter_mapi_endo ~rev_prefix
             (fun ~rev_path:_ d -> if d > 20 then Some (d + 80) else None) t)
       ~union:cantor
       (hook ()) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_filter_map_2 () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check @@ run_result int) "ok"
    (Ok t)
    (Action.run_with_hooks
       ~hooks:(fun () ~rev_prefix t ->
           Result.ok @@ Trie.filter_mapi_endo ~rev_prefix
             (fun ~rev_path:_ x -> Some x) t)
       ~union:cantor
       (hook ()) t)

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
    ];
    "filter_map", [
      test_case "filter_map" `Quick test_filter_map_1;
      test_case "filter_map" `Quick test_filter_map_2;
    ];
  ]
