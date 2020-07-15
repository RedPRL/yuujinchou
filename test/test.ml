open Yuujinchou
open Action
open Pattern

let pp_attr = Format.pp_print_bool

let success = ref true

let test default pattern path expected =
  let compiled = Result.get_ok @@ compile ~join:(||) ~meet:(&&) pattern in
  let output = run compiled ~default path in
  if output <> expected then begin
    Format.printf "@.";
    Format.printf " default: %a@." pp_attr default;
    Format.printf " pattern: %a@." (pp_pattern pp_attr) pattern;
    Format.printf "    path: %a@." pp_path path;
    Format.printf "  output: %a@." (pp_matching_result pp_attr) output;
    Format.printf "expected: %a@." (pp_matching_result pp_attr) expected;
    success := false
  end

let matched l = `Matched l
let nomatch = `NoMatch

;;
let () = test true any [] @@ matched [[], true]
;;
let () = test true any ["a"] @@ matched [["a"], true]
;;
let () = test true any ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (only []) [] @@ matched [[], true]
;;
let () = test true (only []) ["a"] nomatch
;;
let () = test true (only ["a"]) [] nomatch
;;
let () = test true (only ["a"]) ["a"] @@ matched [["a"], true]
;;
let () = test true (only ["a"]) ["b"] nomatch
;;
let () = test true (only ["a"]) ["a"; "b"] nomatch
;;
let () = test true (only ["a"; "b"]) ["a"] nomatch
;;
let () = test true (only ["a"]) ["b"; "c"] nomatch
;;
let () = test true (only ["a"; "b"]) ["c"] nomatch
;;
let () = test true (only ["a"; "b"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (only ["a"; "b"]) ["a"; "c"] nomatch
;;
let () = test true (only ["a"; "b"]) ["c"; "b"] nomatch
;;
let () = test true root [] @@ matched [[], true]
;;
let () = test true root ["a"] nomatch
;;
let () = test true root ["a"; "b"] nomatch
;;
let () = test true wildcard [] nomatch
;;
let () = test true wildcard ["a"] @@ matched [["a"], true]
;;
let () = test true wildcard ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (prefix []) [] @@ matched [[], true]
;;
let () = test true (prefix []) ["a"] @@ matched [["a"], true]
;;
let () = test true (prefix ["a"]) [] nomatch
;;
let () = test true (prefix ["a"]) ["a"] @@ matched [["a"], true]
;;
let () = test true (prefix ["a"]) ["b"] nomatch
;;
let () = test true (prefix []) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (prefix ["a"; "b"]) [] nomatch
;;
let () = test true (prefix ["a"]) ["b"; "c"] nomatch
;;
let () = test true (prefix ["a"; "b"]) ["c"] nomatch
;;
let () = test true (prefix ["a"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (prefix ["a"; "b"]) ["a"] nomatch
;;
let () = test true (prefix ["a"; "b"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (prefix ["a"; "b"]) ["a"; "c"] nomatch
;;
let () = test true (prefix ["a"; "b"]) ["c"; "b"] nomatch
;;
let () = test true (scope [] any) [] @@ matched [[], true]
;;
let () = test true (scope [] root) [] @@ matched [[], true]
;;
let () = test true (scope [] none) [] @@ nomatch
;;
let () = test true (scope [] wildcard) [] @@ nomatch
;;
let () = test true (scope [] @@ renaming [] ["a"]) [] @@ matched [["a"], true]
;;
let () = test true (scope [] any) ["a"] @@ matched [["a"], true]
;;
let () = test true (scope [] root) ["a"] nomatch
;;
let () = test true (scope [] none) ["a"] nomatch
;;
let () = test true (scope [] wildcard) ["a"] @@ matched [["a"], true]
;;
let () = test true (scope [] @@ renaming ["a"] ["b"]) ["a"] @@ matched [["b"], true]
;;
let () = test true (scope ["a"] any) [] nomatch
;;
let () = test true (scope ["a"] root) [] nomatch
;;
let () = test true (scope ["a"] none) [] nomatch
;;
let () = test true (scope ["a"] wildcard) [] nomatch
;;
let () = test true (scope ["a"] @@ renaming [] ["b"]) [] nomatch
;;
let () = test true (scope ["a"] any) ["a"] @@ matched [["a"], true]
;;
let () = test true (scope ["a"] root) ["a"] @@ matched [["a"], true]
;;
let () = test true (scope ["a"] none) ["a"] nomatch
;;
let () = test true (scope ["a"] wildcard) ["a"] nomatch
;;
let () = test true (scope ["a"] @@ renaming ["a"] ["b"]) ["a"] nomatch
;;
let () = test true (scope ["a"] any) ["b"] nomatch
;;
let () = test true (scope ["a"] root) ["b"] nomatch
;;
let () = test true (scope ["a"] none) ["b"] nomatch
;;
let () = test true (scope ["a"] wildcard) ["b"] nomatch
;;
let () = test true (scope ["a"] @@ renaming ["b"] ["c"]) ["b"] nomatch
;;
let () = test true (scope ["a"] any) ["b"; "c"] nomatch
;;
let () = test true (scope ["a"] root) ["b"; "c"] nomatch
;;
let () = test true (scope ["a"] none) ["b"; "c"] nomatch
;;
let () = test true (scope ["a"] wildcard) ["b"; "c"] nomatch
;;
let () = test true (scope ["a"] @@ renaming ["b"] ["c"]) ["b"; "c"] nomatch
;;
let () = test true (scope ["a"; "b"] any) ["c"] nomatch
;;
let () = test true (scope ["a"; "b"] root) ["c"] nomatch
;;
let () = test true (scope ["a"; "b"] none) ["c"] nomatch
;;
let () = test true (scope ["a"; "b"] wildcard) ["c"] nomatch
;;
let () = test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["c"] nomatch
;;
let () = test true (scope ["a"] any) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (scope ["a"] root) ["a"; "b"] nomatch
;;
let () = test true (scope ["a"] none) ["a"; "b"] nomatch
;;
let () = test true (scope ["a"] wildcard) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (scope ["a"] @@ renaming ["b"] ["c"]) ["a"; "b"] @@ matched [["a"; "c"], true]
;;
let () = test true (scope ["a"; "b"] any) ["a"] nomatch
;;
let () = test true (scope ["a"; "b"] root) ["a"] nomatch
;;
let () = test true (scope ["a"; "b"] none) ["a"] nomatch
;;
let () = test true (scope ["a"; "b"] wildcard) ["a"] nomatch
;;
let () = test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["a"] nomatch
;;
let () = test true (scope ["a"; "b"] any) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (scope ["a"; "b"] root) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (scope ["a"; "b"] none) ["a"; "b"] nomatch
;;
let () = test true (scope ["a"; "b"] wildcard) ["a"; "b"] nomatch
;;
let () = test true (scope ["a"; "b"] @@ renaming [] ["c"]) ["a"; "b"] @@ matched [["a"; "b"; "c"], true]
;;
let () = test true (scope ["a"; "b"] any) ["a"; "c"] nomatch
;;
let () = test true (scope ["a"; "b"] root) ["a"; "c"] nomatch
;;
let () = test true (scope ["a"; "b"] none) ["a"; "c"] nomatch
;;
let () = test true (scope ["a"; "b"] wildcard) ["a"; "c"] nomatch
;;
let () = test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["a"; "c"] nomatch
;;
let () = test true (scope ["a"; "b"] any) ["c"; "b"] nomatch
;;
let () = test true (scope ["a"; "b"] root) ["c"; "b"] nomatch
;;
let () = test true (scope ["a"; "b"] none) ["c"; "b"] nomatch
;;
let () = test true (scope ["a"; "b"] wildcard) ["c"; "b"] nomatch
;;
let () = test true (scope ["a"; "b"] @@ renaming ["b"] ["c"]) ["c"; "b"] nomatch
;;
let () = test true none [] nomatch
;;
let () = test true none ["a"] nomatch
;;
let () = test true none ["a"; "b"] nomatch
;;
let () = test true (except []) [] nomatch
;;
let () = test true (except []) ["a"] @@ matched [["a"], true]
;;
let () = test true (except ["a"]) [] @@ matched [[], true]
;;
let () = test true (except ["a"]) ["a"] nomatch
;;
let () = test true (except ["a"]) ["b"] @@ matched [["b"], true]
;;
let () = test true (except ["a"; "b"]) ["a"] @@ matched [["a"], true]
;;
let () = test true (except ["a"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (except ["a"]) ["b"; "c"] @@ matched [["b"; "c"], true]
;;
let () = test true (except ["a"; "b"]) ["c"] @@ matched [["c"], true]
;;
let () = test true (except ["a"; "b"]) ["a"; "b"] nomatch
;;
let () = test true (except ["a"; "b"]) ["a"; "c"] @@ matched [["a"; "c"], true]
;;
let () = test true (except ["a"; "b"]) ["c"; "b"] @@ matched [["c"; "b"], true]
;;
let () = test true (except_prefix []) [] nomatch
;;
let () = test true (except_prefix []) ["a"] nomatch
;;
let () = test true (except_prefix ["a"]) [] @@ matched [[], true]
;;
let () = test true (except_prefix ["a"]) ["a"] nomatch
;;
let () = test true (except_prefix []) ["a"; "b"] nomatch
;;
let () = test true (except_prefix ["a"; "b"]) [] @@ matched [[], true]
;;
let () = test true (except_prefix ["a"]) ["b"] @@ matched [["b"], true]
;;
let () = test true (except_prefix ["a"; "b"]) ["a"] @@ matched [["a"], true]
;;
let () = test true (except_prefix ["a"]) ["a"; "b"] nomatch
;;
let () = test true (except_prefix ["a"]) ["b"; "c"] @@ matched [["b"; "c"], true]
;;
let () = test true (except_prefix ["a"; "b"]) ["c"] @@ matched [["c"], true]
;;
let () = test true (except_prefix ["a"; "b"]) ["a"; "b"] nomatch
;;
let () = test true (except_prefix ["a"; "b"]) ["a"; "c"] @@ matched [["a"; "c"], true]
;;
let () = test true (except_prefix ["a"; "b"]) ["c"; "b"] @@ matched [["c"; "b"], true]
;;
let () = test true (renaming [] []) [] @@ matched [[], true]
;;
let () = test true (renaming [] ["a"]) [] @@ matched [["a"], true]
;;
let () = test true (renaming [] []) ["a"] nomatch
;;
let () = test true (renaming [] ["a"]) ["a"] nomatch
;;
let () = test true (renaming [] ["a"]) ["b"] nomatch
;;
let () = test true (renaming ["a"] []) [] nomatch
;;
let () = test true (renaming ["a"] ["a"]) [] nomatch
;;
let () = test true (renaming ["a"] ["b"]) [] nomatch
;;
let () = test true (renaming ["a"] []) ["a"] @@ matched [[], true]
;;
let () = test true (renaming ["a"] ["a"]) ["a"] @@ matched [["a"], true]
;;
let () = test true (renaming ["a"] ["b"]) ["a"] @@ matched [["b"], true]
;;
let () = test true (renaming ["a"] []) ["b"] nomatch
;;
let () = test true (renaming ["a"] ["a"]) ["b"] nomatch
;;
let () = test true (renaming ["a"] ["b"]) ["b"] nomatch
;;
let () = test true (renaming ["a"] ["b"]) ["c"] nomatch
;;
let () = test true (renaming ["a"] []) ["a"; "b"] nomatch
;;
let () = test true (renaming ["a"] ["a"]) ["a"; "b"] nomatch
;;
let () = test true (renaming ["a"] ["b"]) ["a"; "b"] nomatch
;;
let () = test true (renaming ["a"] ["b"]) ["a"; "c"] nomatch
;;
let () = test true (renaming ["a"; "b"] []) ["a"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["a"]) ["a"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["b"]) ["a"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["a"] nomatch
;;
let () = test true (renaming ["a"] []) ["b"; "c"] nomatch
;;
let () = test true (renaming ["a"] ["a"]) ["b"; "c"] nomatch
;;
let () = test true (renaming ["a"] ["b"]) ["b"; "c"] nomatch
;;
let () = test true (renaming ["a"] ["b"]) ["c"; "b"] nomatch
;;
let () = test true (renaming ["a"] ["b"]) ["c"; "d"] nomatch
;;
let () = test true (renaming ["a"; "b"] []) ["c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["a"]) ["c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["b"]) ["c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["d"] nomatch
;;
let () = test true (renaming ["a"; "b"] []) ["a"; "b"] @@ matched [[], true]
;;
let () = test true (renaming ["a"; "b"] ["a"]) ["a"; "b"] @@ matched [["a"], true]
;;
let () = test true (renaming ["a"; "b"] ["b"]) ["a"; "b"] @@ matched [["b"], true]
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["a"; "b"] @@ matched [["c"], true]
;;
let () = test true (renaming ["a"; "b"] []) ["a"; "c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["a"]) ["a"; "c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["b"]) ["a"; "c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["a"; "c"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["a"; "d"] nomatch
;;
let () = test true (renaming ["a"; "b"] []) ["c"; "b"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["a"]) ["c"; "b"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["b"]) ["c"; "b"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["c"; "b"] nomatch
;;
let () = test true (renaming ["a"; "b"] ["c"]) ["d"; "b"] nomatch
;;
let () = test true (renaming_prefix [] []) [] @@ matched [[], true]
;;
let () = test true (renaming_prefix [] ["a"]) [] @@ matched [["a"], true]
;;
let () = test true (renaming_prefix [] []) ["a"] @@ matched [["a"], true]
;;
let () = test true (renaming_prefix [] ["a"]) ["a"] @@ matched [["a"; "a"], true]
;;
let () = test true (renaming_prefix [] ["a"]) ["b"] @@ matched [["a"; "b"], true]
;;
let () = test true (renaming_prefix ["a"] []) [] nomatch
;;
let () = test true (renaming_prefix ["a"] ["a"]) [] nomatch
;;
let () = test true (renaming_prefix ["a"] ["b"]) [] nomatch
;;
let () = test true (renaming_prefix ["a"] []) ["a"] @@ matched [[], true]
;;
let () = test true (renaming_prefix ["a"] ["a"]) ["a"] @@ matched [["a"], true]
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["a"] @@ matched [["b"], true]
;;
let () = test true (renaming_prefix ["a"] []) ["b"] nomatch
;;
let () = test true (renaming_prefix ["a"] ["a"]) ["b"] nomatch
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["b"] nomatch
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["c"] nomatch
;;
let () = test true (renaming_prefix ["a"] []) ["a"; "b"] @@ matched [["b"], true]
;;
let () = test true (renaming_prefix ["a"] ["a"]) ["a"; "b"] @@ matched [["a"; "b"], true]
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["a"; "b"] @@ matched [["b"; "b"], true]
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["a"; "c"] @@ matched [["b"; "c"], true]
;;
let () = test true (renaming_prefix ["a"; "b"] []) ["a"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["a"]) ["a"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["b"]) ["a"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["a"] nomatch
;;
let () = test true (renaming_prefix ["a"] []) ["b"; "c"] nomatch
;;
let () = test true (renaming_prefix ["a"] ["a"]) ["b"; "c"] nomatch
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["b"; "c"] nomatch
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["c"; "b"] nomatch
;;
let () = test true (renaming_prefix ["a"] ["b"]) ["c"; "d"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] []) ["c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["a"]) ["c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["b"]) ["c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["d"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] []) ["a"; "b"] @@ matched [[], true]
;;
let () = test true (renaming_prefix ["a"; "b"] ["a"]) ["a"; "b"] @@ matched [["a"], true]
;;
let () = test true (renaming_prefix ["a"; "b"] ["b"]) ["a"; "b"] @@ matched [["b"], true]
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["a"; "b"] @@ matched [["c"], true]
;;
let () = test true (renaming_prefix ["a"; "b"] []) ["a"; "c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["a"]) ["a"; "c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["b"]) ["a"; "c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["a"; "c"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["a"; "d"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] []) ["c"; "b"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["a"]) ["c"; "b"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["b"]) ["c"; "b"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["c"; "b"] nomatch
;;
let () = test true (renaming_prefix ["a"; "b"] ["c"]) ["d"; "b"] nomatch
;;
let () = test true (renaming_scope [] ["x"] any) [] @@ matched [["x"], true]
;;
let () = test true (renaming_scope [] ["x"] root) [] @@ matched [["x"], true]
;;
let () = test true (renaming_scope [] ["x"] none) [] @@ nomatch
;;
let () = test true (renaming_scope [] ["x"] wildcard) [] @@ nomatch
;;
let () = test true (renaming_scope [] ["x"] @@ renaming [] ["a"]) [] @@ matched [["x"; "a"], true]
;;
let () = test true (renaming_scope [] ["x"] any) ["a"] @@ matched [["x"; "a"], true]
;;
let () = test true (renaming_scope [] ["x"] root) ["a"] nomatch
;;
let () = test true (renaming_scope [] ["x"] none) ["a"] nomatch
;;
let () = test true (renaming_scope [] ["x"] wildcard) ["a"] @@ matched [["x"; "a"], true]
;;
let () = test true (renaming_scope [] ["x"] @@ renaming ["a"] ["b"]) ["a"] @@ matched [["x"; "b"], true]
;;
let () = test true (renaming_scope ["a"] ["x"] any) [] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] root) [] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] none) [] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] wildcard) [] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] @@ renaming [] ["b"]) [] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] any) ["a"] @@ matched [["x"], true]
;;
let () = test true (renaming_scope ["a"] ["x"] root) ["a"] @@ matched [["x"], true]
;;
let () = test true (renaming_scope ["a"] ["x"] none) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] wildcard) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] @@ renaming ["a"] ["b"]) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] any) ["b"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] root) ["b"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] none) ["b"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] wildcard) ["b"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] @@ renaming ["b"] ["c"]) ["b"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] any) ["b"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] root) ["b"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] none) ["b"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] wildcard) ["b"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] @@ renaming ["b"] ["c"]) ["b"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] any) ["c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] root) ["c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] none) ["c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["c"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] any) ["a"; "b"] @@ matched [["x"; "b"], true]
;;
let () = test true (renaming_scope ["a"] ["x"] root) ["a"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] none) ["a"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"] ["x"] wildcard) ["a"; "b"] @@ matched [["x"; "b"], true]
;;
let () = test true (renaming_scope ["a"] ["x"] @@ renaming ["b"] ["c"]) ["a"; "b"] @@ matched [["x"; "c"], true]
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] any) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] root) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] none) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["a"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] any) ["a"; "b"] @@ matched [["x"], true]
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] root) ["a"; "b"] @@ matched [["x"], true]
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] none) ["a"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["a"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] @@ renaming [] ["c"]) ["a"; "b"] @@ matched [["x"; "c"], true]
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] any) ["a"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] root) ["a"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] none) ["a"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["a"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["a"; "c"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] any) ["c"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] root) ["c"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] none) ["c"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] wildcard) ["c"; "b"] nomatch
;;
let () = test true (renaming_scope ["a"; "b"] ["x"] @@ renaming ["b"] ["c"]) ["c"; "b"] nomatch
;;
let () = test true (attr false any) [] @@ matched [[], false]
;;
let () = test true (attr false (attr true any)) [] @@ matched [[], true]
;;
let () = test true (attr false (attr true (attr false any))) [] @@ matched [[], false]
;;
let () = test true (attr true (attr false (attr true (attr false any)))) [] @@ matched [[], false]
;;
let () = test true (attr true (attr false (attr true (attr false (attr false any))))) [] @@ matched [[], false]
;;
(* XXX More work needed for [seq] *)
;;
let () = test true (seq []) ["a"] nomatch
;;
let () = test true (seq [any]) ["a"] @@ matched [["a"], true]
;;
let () = test true (seq [none]) ["a"] nomatch
;;
let () = test false (seq [join [only ["a"]; renaming ["a"] ["b"]]]) ["a"] @@ matched [["a"], false; ["b"], false]
;;
let () = test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; renaming ["b"] ["a"]]) ["a"] @@ matched [["a"], false]
;;
let () = test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (only ["b"])]) ["a"] @@ matched [["a"], false; ["b"], true]
;;
let () = test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["a"] @@ matched [["a"], true]
;;
let () = test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["b"] @@ matched [["a"], true]
;;
let () = test false (seq [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["c"] nomatch
;;
let () = test true (seq [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["a"] @@ matched [["c"], true]
;;
let () = test true (seq [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["b"] @@ matched [["c"], true]
;;
let () = test true (seq [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["c"] nomatch
;;
(* XXX More work needed for [seq_filter] *)
;;
let () = test true (seq_filter []) ["a"] @@ matched [["a"], true]
;;
let () = test true (seq_filter [any]) ["a"] @@ matched [["a"], true]
;;
let () = test true (seq_filter [none]) ["a"] nomatch
;;
let () = test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]]) ["a"] @@ matched [["a"], false; ["b"], false]
;;
let () = test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; renaming ["b"] ["a"]]) ["a"] @@ matched [["a"], false]
;;
let () = test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (only ["b"])]) ["a"] @@ matched [["b"], true]
;;
let () = test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["a"] @@ matched [["a"], true]
;;
let () = test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["b"] nomatch
;;
let () = test false (seq_filter [join [only ["a"]; renaming ["a"] ["b"]]; attr true (renaming ["b"] ["a"])]) ["c"] nomatch
;;
let () = test true (seq_filter [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["a"] @@ matched [["c"], true]
;;
let () = test true (seq_filter [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["b"] nomatch
;;
let () = test true (seq_filter [renaming ["a"] ["b"]; renaming ["b"] ["c"]]) ["c"] nomatch
;;
(* XXX More work needed for [join] *)
;;
let () = test true (join []) ["a"] nomatch
;;
let () = test true (join [any]) ["a"] @@ matched [["a"], true]
;;
let () = test true (join [none]) ["a"] nomatch
;;
let () = test true (join [wildcard; root]) [] @@ matched [[], true]
;;
let () = test true (join [wildcard; root]) ["a"] @@ matched [["a"], true]
;;
let () = test true (join [any; renaming ["a"] ["b"]]) ["a"] @@ matched [["a"], true; ["b"], true]
;;
let () = test true (join [any; renaming ["a"] ["b"]]) ["b"] @@ matched [["b"], true]
;;
let () = test true (join [any; attr false any]) ["a"] @@ matched [["a"], true]
;;
let () = test false (join [any; attr true any]) ["a"] @@ matched [["a"], true]
;;
(* XXX More work needed for [meet] *)
;;
let () = test true (meet [any]) ["a"] @@ matched [["a"], true]
;;
let () = test true (meet [none]) ["a"] nomatch
;;
let () = test true (meet [wildcard; root]) [] nomatch
;;
let () = test true (meet [wildcard; root]) ["a"] nomatch
;;
let () = test true (meet [any; renaming ["a"] ["b"]]) ["a"] @@ matched []
;;
let () = test true (meet [any; renaming ["a"] ["b"]]) ["b"] nomatch
;;
let () = test true (meet [any; attr false any]) ["a"] @@ matched [["a"], false]
;;
let () = test false (meet [any; attr true any]) ["a"] @@ matched [["a"], false]
;;
(* some "real" test cases *)
;;
let () = test true (join [any; renaming_prefix [] ["M"]]) ["x"] @@ matched [["M";"x"], true; ["x"], true]
;;
let () = test true (join [only ["x"]; only ["y"]]) ["x"] @@ matched [["x"], true]
;;
let () = test true (join [only ["x"]; only ["y"]]) ["y"] @@ matched [["y"], true]
;;
let () = test true (join [only ["x"]; only ["y"]]) ["z"] nomatch
;;
let () = test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["x"] nomatch
;;
let () = test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["y"] nomatch
;;
let () = test true (renaming_scope [] ["M"] @@ meet [except ["x"]; except ["y"]]) ["z"] @@ matched [["M"; "z"], true]
;;
if !success then
  Format.printf "All tests passed.@."
else begin
  Format.printf "@.";
  Format.printf "Some tests failed.@.";
  failwith "testing failed"
end
