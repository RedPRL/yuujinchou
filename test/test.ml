open Yuujinchou
open Action
open Pattern

let pp_attr = Format.pp_print_bool

let success = ref true

let test default pattern path expected =
  let output = run ~default ~join:(||) ~meet:(&&) pattern path in
  if output <> expected then begin
    Format.printf "@.";
    Format.printf " default: %a@." pp_attr default;
    Format.printf " pattern: %a@." (pp_pattern pp_attr) pattern;
    Format.printf "    path: %a@." pp_path path;
    Format.printf "  output: %a@." (pp_result pp_attr) output;
    Format.printf "expected: %a@." (pp_result pp_attr) expected;
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
(* XXX More work needed for [seq] *)
;;
test true (seq []) ["a"] nomatch
;;
test true (seq [any]) ["a"] @@ matched [["a"], true]
;;
test true (seq [none]) ["a"] nomatch
;;
test false (seq [join [only ["a"]; renaming ["a"] ["b"]]]) ["a"] @@ matched [["a"], false; ["b"], false]
;;
test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; renaming ["b"] ["a"]]) ["a"] @@ matched [["a"], false]
;;
test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (only ["b"])]) ["a"] @@ matched [["a"], false; ["b"], true]
;;
test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["a"] @@ matched [["a"], true]
;;
test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["b"] @@ matched [["a"], true]
;;
test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["c"] nomatch
;;
test true (seq [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["a"] @@ matched [["c"], true]
;;
test true (seq [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["b"] @@ matched [["c"], true]
;;
test true (seq [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["c"] nomatch
;;
(* XXX More work needed for [seq_filter] *)
;;
test true (seq_filter []) ["a"] @@ matched [["a"], true]
;;
test true (seq_filter [any]) ["a"] @@ matched [["a"], true]
;;
test true (seq_filter [none]) ["a"] nomatch
;;
test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]]) ["a"] @@ matched [["a"], false; ["b"], false]
;;
test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; renaming ["b"] ["a"]]) ["a"] @@ matched [["a"], false]
;;
test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (only ["b"])]) ["a"] @@ matched [["b"], true]
;;
test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["a"] @@ matched [["a"], true]
;;
test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["b"] nomatch
;;
test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["c"] nomatch
;;
test true (seq_filter [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["a"] @@ matched [["c"], true]
;;
test true (seq_filter [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["b"] nomatch
;;
test true (seq_filter [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["c"] nomatch
;;
(* XXX More work needed for [join] *)
;;
test true (join []) ["a"] nomatch
;;
test true (join [any]) ["a"] @@ matched [["a"], true]
;;
test true (join [none]) ["a"] nomatch
;;
test true (join [wildcard; root]) [] @@ matched [[], true]
;;
test true (join [wildcard; root]) ["a"] @@ matched [["a"], true]
;;
test true (join [any; renaming ["a"] ["b"]]) ["a"] @@ matched [["a"], true; ["b"], true]
;;
test true (join [any; renaming ["a"] ["b"]]) ["b"] @@ matched [["b"], true]
;;
test true (join [any; attr false any]) ["a"] @@ matched [["a"], true]
;;
test false (join [any; attr true any]) ["a"] @@ matched [["a"], true]
;;
(* XXX More work needed for [meet] *)
;;
test true (meet [any]) ["a"] @@ matched [["a"], true]
;;
test true (meet [none]) ["a"] nomatch
;;
test true (meet [wildcard; root]) [] nomatch
;;
test true (meet [wildcard; root]) ["a"] nomatch
;;
test true (meet [any; renaming ["a"] ["b"]]) ["a"] @@ matched []
;;
test true (meet [any; renaming ["a"] ["b"]]) ["b"] nomatch
;;
test true (meet [any; attr false any]) ["a"] @@ matched [["a"], false]
;;
test false (meet [any; attr true any]) ["a"] @@ matched [["a"], false]
;;
(* some "real" test cases *)
;;
test true (join [any; renaming_prefix [] ["M"]]) ["x"] @@ matched [["M";"x"], true; ["x"], true]
;;
test true (join [only ["x"]; only ["y"]]) ["x"] @@ matched [["x"], true]
;;
test true (join [only ["x"]; only ["y"]]) ["y"] @@ matched [["y"], true]
;;
test true (join [only ["x"]; only ["y"]]) ["z"] nomatch
;;
test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["x"] nomatch
;;
test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["y"] nomatch
;;
test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["z"] @@ matched [["M"; "z"], true]
;;
if !success then
  Format.printf "All tests passed.@."
else begin
  Format.printf "@.";
  Format.printf "Some tests failed.@.";
  failwith "testing failed"
end
