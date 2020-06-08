open Yuujinchou
open Action
open Pattern

let pp_attr = Format.pp_print_bool

let test default pattern path expected =
  Format.printf "@.";
  Format.printf "Input Default:   %a@." pp_attr default;
  Format.printf "Input Pattern:   %a@." (pp_pattern pp_attr) pattern;
  Format.printf "Input Path:      %a@." pp_path path;
  Format.printf "Expected Output: %a@." (pp_result pp_attr) expected;
  let output = run ~default ~join:(||) ~meet:(&&) pattern path in
  Format.printf "Actual Output:   %a@." (pp_result pp_attr) output;
  assert (output = expected)

let _ =
  test true any ["test"] @@
  Ok (`Matched [["test"], true])

let _ =
  test true none ["test"] @@
  Ok `NoMatch

let _ =
  test true (renaming ["test"] ["test1"]) ["test"] @@
  Ok (`Matched [["test1"], true])

let _ =
  test true (join [renaming ["test"] ["test1"]; renaming ["test"] ["test2"]]) ["test"] @@
  Ok (`Matched [["test1"], true; ["test2"], true])

let _ =
  test true (join [any; renaming_prefix [] ["M"]]) ["test"] @@
  Ok (`Matched [["M";"test"], true; ["test"], true])

let _ =
  test true (join [id ["x"]; id ["y"]]) ["x"] @@
  Ok (`Matched [["x"], true])

let _ =
  test true (renaming_scope [] ["M"] @@ meet [skip ["x"]; skip ["y"]]) ["y"] @@
  Ok `NoMatch

let _ =
  test true (renaming_scope [] ["M"] @@ meet [skip ["x"]; skip ["y"]]) ["z"] @@
  Ok (`Matched [["M"; "z"], true])
