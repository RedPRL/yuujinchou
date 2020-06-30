open Yuujinchou
open Action
open Pattern

let pp_attr = Format.pp_print_bool

let test default pattern path expected =
  let output = run ~default ~join:(||) ~meet:(&&) pattern path in
  if output <> expected then begin
    Format.printf "@.";
    Format.printf "Input Default:   %a@." pp_attr default;
    Format.printf "Input Pattern:   %a@." (pp_pattern pp_attr) pattern;
    Format.printf "Input Path:      %a@." pp_path path;
    Format.printf "Output:          %a@." (pp_result pp_attr) output;
    Format.printf "Expected Output: %a@." (pp_result pp_attr) expected;
    failwith "testing failed"
  end

let matched l = Ok (`Matched l)
let nomatch = Ok `NoMatch

;;
test true any [] @@ matched [[], true]
;;
test true any ["a"] @@ matched [["a"], true]
;;
test true any ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (only []) [] @@ matched [[], true]
;;
test true (only []) ["a"] nomatch
;;
test true (only ["a"]) [] nomatch
;;
test true (only ["a"]) ["a"] @@ matched [["a"], true]
;;
test true (only ["a"]) ["b"] nomatch
;;
test true (only ["a"]) ["a"; "b"] nomatch
;;
test true (only ["a"; "b"]) ["a"] nomatch
;;
test true (only ["a"]) ["b"; "c"] nomatch
;;
test true (only ["a"; "b"]) ["c"] nomatch
;;
test true (only ["a"; "b"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (only ["a"; "b"]) ["a"; "c"] nomatch
;;
test true (only ["a"; "b"]) ["c"; "b"] nomatch
;;
test true root [] @@ matched [[], true]
;;
test true root ["a"] nomatch
;;
test true root ["a"; "b"] nomatch
;;
test true wildcard [] nomatch
;;
test true wildcard ["a"] @@ matched [["a"], true]
;;
test true wildcard ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (prefix []) [] @@ matched [[], true]
;;
test true (prefix []) ["a"] @@ matched [["a"], true]
;;
test true (prefix ["a"]) [] nomatch
;;
test true (prefix ["a"]) ["a"] @@ matched [["a"], true]
;;
test true (prefix ["a"]) ["b"] nomatch
;;
test true (prefix ["a"]) ["b"; "c"] nomatch
;;
test true (prefix ["a"; "b"]) ["c"] nomatch
;;
test true (prefix ["a"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (prefix ["a"; "b"]) ["a"] nomatch
;;
test true (prefix ["a"; "b"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (prefix ["a"; "b"]) ["a"; "c"] nomatch
;;
test true (prefix ["a"; "b"]) ["c"; "b"] nomatch
;;
test true (scope [] any) [] @@ matched [[], true]
;;
test true (scope [] any) ["a"] @@ matched [["a"], true]
;;
test true (scope [] @@ only ["a"]) ["b"] nomatch
;;
test true (scope [] none) ["a"] nomatch
;;
test true (scope ["a"] any) [] nomatch
;;
test true (scope ["a"] any) ["a"] @@ matched [["a"], true]
;;
test true (scope ["a"] none) ["a"] nomatch
;;
test true (scope ["a"] any) ["b"] nomatch
;;
test true (scope ["a"] any) ["b"; "c"] nomatch
;;
test true (scope ["a"; "b"] any) ["a"] nomatch
;;
test true (scope ["a"; "b"] any) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (scope ["a"; "b"] root) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (scope ["a"; "b"] none) ["a"; "b"] nomatch
;;
test true (scope ["a"; "b"] wildcard) ["a"; "b"] nomatch
;;
test true (scope ["a"; "b"] root) ["a"; "b"; "c"] nomatch
;;
test true (scope ["a"; "b"] @@ only ["c"]) ["a"; "b"; "c"] @@ matched [["a"; "b"; "c"], true]
;;
test true none [] nomatch
;;
test true none ["a"] nomatch
;;
test true none ["a"; "b"] nomatch
;;
test true (except []) [] nomatch
;;
test true (except []) ["a"] @@ matched [["a"], true]
;;
test true (except ["a"]) [] @@ matched [[], true]
;;
test true (except ["a"]) ["a"] nomatch
;;
test true (except ["a"]) ["b"] @@ matched [["b"], true]
;;
test true (except ["a"; "b"]) ["a"] @@ matched [["a"], true]
;;
test true (except ["a"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (except ["a"]) ["b"; "c"] @@ matched [["b"; "c"], true]
;;
test true (except ["a"; "b"]) ["c"] @@ matched [["c"], true]
;;
test true (except ["a"; "b"]) ["a"; "b"] nomatch
;;
test true (except ["a"; "b"]) ["a"; "c"] @@ matched [["a"; "c"], true]
;;
test true (except ["a"; "b"]) ["c"; "b"] @@ matched [["c"; "b"], true]
;;
test true (except_prefix []) [] nomatch
;;
test true (except_prefix []) ["a"] nomatch
;;
test true (except_prefix ["a"]) [] @@ matched [[], true]
;;
test true (except_prefix ["a"]) ["a"] nomatch
;;
test true (except_prefix ["a"]) ["b"] @@ matched [["b"], true]
;;
test true (except_prefix ["a"; "b"]) ["a"] @@ matched [["a"], true]
;;
test true (except_prefix ["a"]) ["a"; "b"] nomatch
;;
test true (except_prefix ["a"]) ["b"; "c"] @@ matched [["b"; "c"], true]
;;
test true (except_prefix ["a"; "b"]) ["c"] @@ matched [["c"], true]
;;
test true (except_prefix ["a"; "b"]) ["a"; "b"] nomatch
;;
test true (except_prefix ["a"; "b"]) ["a"; "c"] @@ matched [["a"; "c"], true]
;;
test true (except_prefix ["a"; "b"]) ["c"; "b"] @@ matched [["c"; "b"], true]
;;
test true (renaming [] []) [] @@ matched [[], true]
;;
test true (renaming [] ["a"]) [] @@ matched [["a"], true]
;;
test true (renaming [] []) ["a"] nomatch
;;
test true (renaming [] ["a"]) ["a"] nomatch
;;
test true (renaming [] ["a"]) ["b"] nomatch
;;
test true (renaming ["a"] []) [] nomatch
;;
test true (renaming ["a"] ["a"]) [] nomatch
;;
test true (renaming ["a"] ["b"]) [] nomatch
;;
test true (renaming ["a"] []) ["a"] @@ matched [[], true]
;;
test true (renaming ["a"] ["a"]) ["a"] @@ matched [["a"], true]
;;
test true (renaming ["a"] ["b"]) ["a"] @@ matched [["b"], true]
;;
test true (renaming ["a"] []) ["b"] nomatch
;;
test true (renaming ["a"] ["a"]) ["b"] nomatch
;;
test true (renaming ["a"] ["b"]) ["b"] nomatch
;;
test true (renaming ["a"] ["b"]) ["c"] nomatch
;;
test true (renaming ["a"] []) ["a"; "b"] nomatch
;;
test true (renaming ["a"] ["a"]) ["a"; "b"] nomatch
;;
test true (renaming ["a"] ["b"]) ["a"; "b"] nomatch
;;
test true (renaming ["a"] ["b"]) ["a"; "c"] nomatch
;;
test true (renaming ["a"; "b"] []) ["a"] nomatch
;;
test true (renaming ["a"; "b"] ["a"]) ["a"] nomatch
;;
test true (renaming ["a"; "b"] ["b"]) ["a"] nomatch
;;
test true (renaming ["a"; "b"] ["c"]) ["a"] nomatch
;;
test true (renaming ["a"] []) ["b"; "c"] nomatch
;;
test true (renaming ["a"] ["a"]) ["b"; "c"] nomatch
;;
test true (renaming ["a"] ["b"]) ["b"; "c"] nomatch
;;
test true (renaming ["a"] ["b"]) ["c"; "b"] nomatch
;;
test true (renaming ["a"] ["b"]) ["c"; "d"] nomatch
;;
test true (renaming ["a"; "b"] []) ["c"] nomatch
;;
test true (renaming ["a"; "b"] ["a"]) ["c"] nomatch
;;
test true (renaming ["a"; "b"] ["b"]) ["c"] nomatch
;;
test true (renaming ["a"; "b"] ["c"]) ["c"] nomatch
;;
test true (renaming ["a"; "b"] ["c"]) ["d"] nomatch
;;
test true (renaming ["a"; "b"] []) ["a"; "b"] @@ matched [[], true]
;;
test true (renaming ["a"; "b"] ["a"]) ["a"; "b"] @@ matched [["a"], true]
;;
test true (renaming ["a"; "b"] ["b"]) ["a"; "b"] @@ matched [["b"], true]
;;
test true (renaming ["a"; "b"] ["c"]) ["a"; "b"] @@ matched [["c"], true]
;;
test true (renaming ["a"; "b"] []) ["a"; "c"] nomatch
;;
test true (renaming ["a"; "b"] ["a"]) ["a"; "c"] nomatch
;;
test true (renaming ["a"; "b"] ["b"]) ["a"; "c"] nomatch
;;
test true (renaming ["a"; "b"] ["c"]) ["a"; "c"] nomatch
;;
test true (renaming ["a"; "b"] ["c"]) ["a"; "d"] nomatch
;;
test true (renaming ["a"; "b"] []) ["c"; "b"] nomatch
;;
test true (renaming ["a"; "b"] ["a"]) ["c"; "b"] nomatch
;;
test true (renaming ["a"; "b"] ["b"]) ["c"; "b"] nomatch
;;
test true (renaming ["a"; "b"] ["c"]) ["c"; "b"] nomatch
;;
test true (renaming ["a"; "b"] ["c"]) ["d"; "b"] nomatch
;;
test true (renaming_prefix [] []) [] @@ matched [[], true]
;;
test true (renaming_prefix [] ["a"]) [] @@ matched [["a"], true]
;;
test true (renaming_prefix [] []) ["a"] @@ matched [["a"], true]
;;
test true (renaming_prefix [] ["a"]) ["a"] @@ matched [["a"; "a"], true]
;;
test true (renaming_prefix [] ["a"]) ["b"] @@ matched [["a"; "b"], true]
;;
test true (renaming_prefix ["a"] []) [] nomatch
;;
test true (renaming_prefix ["a"] ["a"]) [] nomatch
;;
test true (renaming_prefix ["a"] ["b"]) [] nomatch
;;
test true (renaming_prefix ["a"] []) ["a"] @@ matched [[], true]
;;
test true (renaming_prefix ["a"] ["a"]) ["a"] @@ matched [["a"], true]
;;
test true (renaming_prefix ["a"] ["b"]) ["a"] @@ matched [["b"], true]
;;
test true (renaming_prefix ["a"] []) ["b"] nomatch
;;
test true (renaming_prefix ["a"] ["a"]) ["b"] nomatch
;;
test true (renaming_prefix ["a"] ["b"]) ["b"] nomatch
;;
test true (renaming_prefix ["a"] ["b"]) ["c"] nomatch
;;
test true (renaming_prefix ["a"] []) ["a"; "b"] @@ matched [["b"], true]
;;
test true (renaming_prefix ["a"] ["a"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (renaming_prefix ["a"] ["b"]) ["a"; "b"] @@ matched [["b"; "b"], true]
;;
test true (renaming_prefix ["a"] ["b"]) ["a"; "c"] @@ matched [["b"; "c"], true]
;;
test true (renaming_prefix ["a"; "b"] []) ["a"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["a"]) ["a"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["b"]) ["a"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["a"] nomatch
;;
test true (renaming_prefix ["a"] []) ["b"; "c"] nomatch
;;
test true (renaming_prefix ["a"] ["a"]) ["b"; "c"] nomatch
;;
test true (renaming_prefix ["a"] ["b"]) ["b"; "c"] nomatch
;;
test true (renaming_prefix ["a"] ["b"]) ["c"; "b"] nomatch
;;
test true (renaming_prefix ["a"] ["b"]) ["c"; "d"] nomatch
;;
test true (renaming_prefix ["a"; "b"] []) ["c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["a"]) ["c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["b"]) ["c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["d"] nomatch
;;
test true (renaming_prefix ["a"; "b"] []) ["a"; "b"] @@ matched [[], true]
;;
test true (renaming_prefix ["a"; "b"] ["a"]) ["a"; "b"] @@ matched [["a"], true]
;;
test true (renaming_prefix ["a"; "b"] ["b"]) ["a"; "b"] @@ matched [["b"], true]
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["a"; "b"] @@ matched [["c"], true]
;;
test true (renaming_prefix ["a"; "b"] []) ["a"; "c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["a"]) ["a"; "c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["b"]) ["a"; "c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["a"; "c"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["a"; "d"] nomatch
;;
test true (renaming_prefix ["a"; "b"] []) ["c"; "b"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["a"]) ["c"; "b"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["b"]) ["c"; "b"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["c"; "b"] nomatch
;;
test true (renaming_prefix ["a"; "b"] ["c"]) ["d"; "b"] nomatch
;;
(* TODO continue working on renaming_scope, attr, seq, seq_filter, join, meet *)
(* TODO clean up the following test cases *)
;;
test true (join [renaming ["test"] ["test1"]; renaming ["test"] ["test2"]]) ["test"] @@
matched [["test1"], true; ["test2"], true]
;;
test true (join [any; renaming_prefix [] ["M"]]) ["test"] @@
matched [["M";"test"], true; ["test"], true]
;;
test true (join [only ["x"]; only ["y"]]) ["x"] @@
matched [["x"], true]
;;
test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["y"] nomatch
;;
test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["z"] @@
matched [["M"; "z"], true]
;;
test true (seq [renaming ["x"] ["y"]; renaming ["y"] ["z"]]) ["x"] @@
matched [["z"], true]
;;
test true (seq [renaming ["x"] ["y"]; renaming ["z"] ["w"]]) ["x"] @@
matched [["y"], true]
;;
Printf.printf "All tests passed."
