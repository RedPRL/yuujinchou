module Hashtbl = struct
  include Hashtbl

  let pp pp_k pp_v fmt t =
    t
    |> Hashtbl.iter @@ fun k v ->
       Format.fprintf fmt "@[%a: %a@]@ " pp_k k pp_v v
end

(** {1 Example Code} *)

open Yuujinchou

type data = int [@@deriving show]

type env = (Pattern.path, data) Hashtbl.t [@@deriving show]
(** An environment is a mapping from paths to data. *)

module DataSet = struct
  include Set.Make (struct
    type t = data

    let compare = compare
  end)

  let pp fmt s = s |> iter @@ fun m -> Format.fprintf fmt "%a@ " pp_data m
end

let collect_matched env pattern =
  let compiled_pattern = Result.get_ok @@ Action.compile_ pattern in
  ( env
  |> Hashtbl.fold @@ fun path data set ->
     match Action.run_ compiled_pattern path with
     | `NoMatch -> set
     | `Matched _ -> DataSet.add data set )
    DataSet.empty

(** {1 Testing} *)

let hashtbl_of_list l = Hashtbl.of_seq @@ List.to_seq l

let list_of_hashtbl t = List.of_seq @@ Hashtbl.to_seq t

let set_of_list l = DataSet.of_seq @@ List.to_seq l

let list_of_set s = List.of_seq @@ DataSet.to_seq s

let differ_set s1 s2 =
  List.sort compare (list_of_set s1) <> List.sort compare (list_of_set s2)

let success = ref true

let pp_attr fmt () = Format.pp_print_string fmt "()"

let test env pattern expected =
  let env' = hashtbl_of_list env and expected' = set_of_list expected in
  let set' = collect_matched env' pattern in
  if differ_set set' expected' then (
    Format.printf "@.";
    Format.printf "     env: %a@." pp_env env';
    Format.printf " pattern: %a@." (Pattern.pp_pattern pp_attr) pattern;
    Format.printf "  output: %a@." DataSet.pp set';
    Format.printf "expected: %a@." DataSet.pp expected';
    success := false )

let () =
  test
    [ ([ "a"; "b" ], 10); ([ "c" ], 20); ([ "a"; "c" ], 30) ]
    Pattern.(seq [ only [ "a" ]; renaming [ "a"; "c" ] [ "c" ] ])
    [ 30 ]

;;
if !success then Format.printf "All tests passed.@."
else (
  Format.printf "@.";
  Format.printf "Some tests failed.@.";
  failwith "testing failed" )
