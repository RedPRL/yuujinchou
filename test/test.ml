open Yuujinchou
open Action
open Pattern

let pp_attr = Format.pp_print_bool

let success = ref true

let test default pattern path expected =
  let output = run ~default ~join:(||) ~meet:(&&) pattern path in
  if output <> expected then begin
    Format.printf "@.";
    Format.printf "Input Default:   %a@." pp_attr default;
    Format.printf "Input Pattern:   %a@." (pp_pattern pp_attr) pattern;
    Format.printf "Input Path:      %a@." pp_path path;
    Format.printf "Output:          %a@." (pp_result pp_attr) output;
    Format.printf "Expected Output: %a@." (pp_result pp_attr) expected;
    success := false
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
test true (prefix []) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (prefix ["a"; "b"]) [] nomatch
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
test true (scope [] root) [] @@ matched [[], true]
;;
test true (scope [] none) [] @@ nomatch
;;
test true (scope [] wildcard) [] @@ nomatch
;;
test true (scope [] @@ renaming [] ["a"]) [] @@ matched [["a"], true]
;;
test true (scope [] any) ["a"] @@ matched [["a"], true]
;;
test true (scope [] root) ["a"] nomatch
;;
test true (scope [] none) ["a"] nomatch
;;
test true (scope [] wildcard) ["a"] @@ matched [["a"], true]
;;
test true (scope [] @@ renaming ["a"] ["b"]) ["a"] @@ matched [["b"], true]
;;
test true (scope ["a"] any) [] nomatch
;;
test true (scope ["a"] root) [] nomatch
;;
test true (scope ["a"] none) [] nomatch
;;
test true (scope ["a"] wildcard) [] nomatch
;;
test true (scope ["a"] @@ renaming [] ["b"]) [] nomatch
;;
test true (scope ["a"] any) ["a"] @@ matched [["a"], true]
;;
test true (scope ["a"] root) ["a"] @@ matched [["a"], true]
;;
test true (scope ["a"] none) ["a"] nomatch
;;
test true (scope ["a"] wildcard) ["a"] nomatch
;;
test true (scope ["a"] @@ renaming ["a"] ["b"]) ["a"] nomatch
;;
test true (scope ["a"] any) ["b"] nomatch
;;
test true (scope ["a"] root) ["b"] nomatch
;;
test true (scope ["a"] none) ["b"] nomatch
;;
test true (scope ["a"] wildcard) ["b"] nomatch
;;
test true (scope ["a"] @@ renaming ["b"] ["c"]) ["b"] nomatch
;;
test true (scope ["a"] any) ["b"; "c"] nomatch
;;
test true (scope ["a"] root) ["b"; "c"] nomatch
;;
test true (scope ["a"] none) ["b"; "c"] nomatch
;;
test true (scope ["a"] wildcard) ["b"; "c"] nomatch
;;
test true (scope ["a"] @@ renaming ["b"] ["c"]) ["b"; "c"] nomatch
;;
test true (scope ["a"; "b"] any) ["c"] nomatch
;;
test true (scope ["a"; "b"] root) ["c"] nomatch
;;
test true (scope ["a"; "b"] none) ["c"] nomatch
;;
test true (scope ["a"; "b"] wildcard) ["c"] nomatch
;;
test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["c"] nomatch
;;
test true (scope ["a"] any) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (scope ["a"] root) ["a"; "b"] nomatch
;;
test true (scope ["a"] none) ["a"; "b"] nomatch
;;
test true (scope ["a"] wildcard) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (scope ["a"] @@ renaming ["b"] ["c"]) ["a"; "b"] @@ matched [["a"; "c"], true]
;;
test true (scope ["a"; "b"] any) ["a"] nomatch
;;
test true (scope ["a"; "b"] root) ["a"] nomatch
;;
test true (scope ["a"; "b"] none) ["a"] nomatch
;;
test true (scope ["a"; "b"] wildcard) ["a"] nomatch
;;
test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["a"] nomatch
;;
test true (scope ["a"; "b"] any) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (scope ["a"; "b"] root) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
test true (scope ["a"; "b"] none) ["a"; "b"] nomatch
;;
test true (scope ["a"; "b"] wildcard) ["a"; "b"] nomatch
;;
test true (scope ["a"; "b"] @@ renaming [] ["c"]) ["a"; "b"] @@ matched [["a"; "b"; "c"], true]
;;
test true (scope ["a"; "b"] any) ["a"; "c"] nomatch
;;
test true (scope ["a"; "b"] root) ["a"; "c"] nomatch
;;
test true (scope ["a"; "b"] none) ["a"; "c"] nomatch
;;
test true (scope ["a"; "b"] wildcard) ["a"; "c"] nomatch
;;
test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["a"; "c"] nomatch
;;
test true (scope ["a"; "b"] any) ["c"; "b"] nomatch
;;
test true (scope ["a"; "b"] root) ["c"; "b"] nomatch
;;
test true (scope ["a"; "b"] none) ["c"; "b"] nomatch
;;
test true (scope ["a"; "b"] wildcard) ["c"; "b"] nomatch
;;
test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["c"; "b"] nomatch
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
test true (except_prefix []) ["a"; "b"] nomatch
;;
test true (except_prefix ["a"; "b"]) [] @@ matched [[], true]
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
test true (renaming_scope [] ["x"] any) [] @@ matched [["x"], true]
;;
test true (renaming_scope [] ["x"] root) [] @@ matched [["x"], true]
;;
test true (renaming_scope [] ["x"] none) [] @@ nomatch
;;
test true (renaming_scope [] ["x"] wildcard) [] @@ nomatch
;;
test true (renaming_scope [] ["x"] @@ renaming [] ["a"]) [] @@ matched [["x"; "a"], true]
;;
test true (renaming_scope [] ["x"] any) ["a"] @@ matched [["x"; "a"], true]
;;
test true (renaming_scope [] ["x"] root) ["a"] nomatch
;;
test true (renaming_scope [] ["x"] none) ["a"] nomatch
;;
test true (renaming_scope [] ["x"] wildcard) ["a"] @@ matched [["x"; "a"], true]
;;
test true (renaming_scope [] ["x"] @@ renaming ["a"] ["b"]) ["a"] @@ matched [["x"; "b"], true]
;;
test true (renaming_scope ["a"] ["x"] any) [] nomatch
;;
test true (renaming_scope ["a"] ["x"] root) [] nomatch
;;
test true (renaming_scope ["a"] ["x"] none) [] nomatch
;;
test true (renaming_scope ["a"] ["x"] wildcard) [] nomatch
;;
test true (renaming_scope ["a"] ["x"] @@ renaming [] ["b"]) [] nomatch
;;
test true (renaming_scope ["a"] ["x"] any) ["a"] @@ matched [["x"], true]
;;
test true (renaming_scope ["a"] ["x"] root) ["a"] @@ matched [["x"], true]
;;
test true (renaming_scope ["a"] ["x"] none) ["a"] nomatch
;;
test true (renaming_scope ["a"] ["x"] wildcard) ["a"] nomatch
;;
test true (renaming_scope ["a"] ["x"] @@ renaming ["a"] ["b"]) ["a"] nomatch
;;
test true (renaming_scope ["a"] ["x"] any) ["b"] nomatch
;;
test true (renaming_scope ["a"] ["x"] root) ["b"] nomatch
;;
test true (renaming_scope ["a"] ["x"] none) ["b"] nomatch
;;
test true (renaming_scope ["a"] ["x"] wildcard) ["b"] nomatch
;;
test true (renaming_scope ["a"] ["x"] @@ renaming ["b"] ["c"]) ["b"] nomatch
;;
test true (renaming_scope ["a"] ["x"] any) ["b"; "c"] nomatch
;;
test true (renaming_scope ["a"] ["x"] root) ["b"; "c"] nomatch
;;
test true (renaming_scope ["a"] ["x"] none) ["b"; "c"] nomatch
;;
test true (renaming_scope ["a"] ["x"] wildcard) ["b"; "c"] nomatch
;;
test true (renaming_scope ["a"] ["x"] @@ renaming ["b"] ["c"]) ["b"; "c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] any) ["c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] root) ["c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] none) ["c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["c"] nomatch
;;
test true (renaming_scope ["a"] ["x"] any) ["a"; "b"] @@ matched [["x"; "b"], true]
;;
test true (renaming_scope ["a"] ["x"] root) ["a"; "b"] nomatch
;;
test true (renaming_scope ["a"] ["x"] none) ["a"; "b"] nomatch
;;
test true (renaming_scope ["a"] ["x"] wildcard) ["a"; "b"] @@ matched [["x"; "b"], true]
;;
test true (renaming_scope ["a"] ["x"] @@ renaming ["b"] ["c"]) ["a"; "b"] @@ matched [["x"; "c"], true]
;;
test true (renaming_scope ["a"; "b"] ["x"] any) ["a"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] root) ["a"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] none) ["a"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["a"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["a"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] any) ["a"; "b"] @@ matched [["x"], true]
;;
test true (renaming_scope ["a"; "b"] ["x"] root) ["a"; "b"] @@ matched [["x"], true]
;;
test true (renaming_scope ["a"; "b"] ["x"] none) ["a"; "b"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["a"; "b"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] @@ renaming [] ["c"]) ["a"; "b"] @@ matched [["x"; "c"], true]
;;
test true (renaming_scope ["a"; "b"] ["x"] any) ["a"; "c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] root) ["a"; "c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] none) ["a"; "c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["a"; "c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["a"; "c"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] any) ["c"; "b"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] root) ["c"; "b"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] none) ["c"; "b"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["c"; "b"] nomatch
;;
test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["c"; "b"] nomatch
;;
test true (attr false any) [] @@ matched [[], false]
;;
test true (attr false (attr true any)) [] @@ matched [[], true]
;;
test true (attr false (attr true (attr false any))) [] @@ matched [[], false]
;;
test true (attr true (attr false (attr true (attr false any)))) [] @@ matched [[], false]
;;
test true (attr true (attr false (attr true (attr false (attr false any))))) [] @@ matched [[], false]
;;
(* TODO continue working on seq, seq_filter, join, meet *)
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
if !success then
  Printf.printf "All tests passed."
else begin
  Printf.printf "Some tests failed.";
  failwith "testing failed"
end
