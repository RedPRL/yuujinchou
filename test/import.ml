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

(** [remap pattern env] uses the [pattern] to massage
    the environment [env]. *)
let remap pattern env =
  let compiled_pattern = Result.get_ok @@ Action.compile_ pattern in
  let new_env = Hashtbl.create @@ Hashtbl.length env in
  ( env
  |> Hashtbl.iter @@ fun path data ->
     match Action.run_ compiled_pattern path with
     | `NoMatch -> ()
     | `Matched l -> (
         l
         |> List.iter @@ fun (path, ()) ->
            match Hashtbl.find_opt new_env path with
            | None -> Hashtbl.replace new_env path data
            | Some data' ->
                if data <> data' then
                  failwith "Inconsistent data assigned to the same path." ) );
  new_env

(** [import env pattern imported] imports the environment
    [imported] massaged by [pattern] into [env]. *)
let import env pattern imported =
  Hashtbl.replace_seq env @@ Hashtbl.to_seq @@ remap pattern imported

(** {1 Testing} *)

let hashtbl_of_list l = Hashtbl.of_seq @@ List.to_seq l

let list_of_hashtbl t = List.of_seq @@ Hashtbl.to_seq t

let differ_hashtbl t1 t2 =
  List.sort compare (list_of_hashtbl t1)
  <> List.sort compare (list_of_hashtbl t2)

let success = ref true

let pp_attr fmt () = Format.pp_print_string fmt "()"

let test env pattern imported expected =
  let env' = hashtbl_of_list env
  and imported' = hashtbl_of_list imported
  and expected' = hashtbl_of_list expected in
  import env' pattern imported';
  let old_env' = hashtbl_of_list env in
  if differ_hashtbl env' expected' then (
    Format.printf "@.";
    Format.printf "     env: %a@." pp_env old_env';
    Format.printf " pattern: %a@." (Pattern.pp_pattern pp_attr) pattern;
    Format.printf "imported: %a@." pp_env imported';
    Format.printf "  output: %a@." pp_env env';
    Format.printf "expected: %a@." pp_env expected';
    success := false )

let () =
  test
    [ ([ "a"; "b" ], 10); ([ "c" ], 20); ([ "a"; "c" ], 30) ]
    Pattern.(seq [ renaming_prefix [] [ "a" ]; renaming [ "a"; "c" ] [ "c" ] ])
    [ ([ "b" ], 100); ([ "c" ], 200); ([ "d" ], 300) ]
    [
      ([ "a"; "b" ], 100);
      ([ "c" ], 200);
      ([ "a"; "c" ], 30);
      ([ "a"; "d" ], 300);
    ]

;;
if !success then Format.printf "All tests passed.@."
else (
  Format.printf "@.";
  Format.printf "Some tests failed.@.";
  failwith "testing failed" )
