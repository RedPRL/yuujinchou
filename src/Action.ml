open StdLabels
open Pattern

type 'a error =
  | ReplacementNotUsed of 'a pattern
  | EmptyMeet of 'a pattern
[@@deriving show]

module M = Map.Make (struct type t = path let compare = compare end)

module Compile :
sig
  type 'a compiled_pattern = 'a -> path -> [ `NoMatch | `Matched of 'a M.t ]
  val compile : join:('a -> 'a -> 'a) -> meet:('a -> 'a -> 'a) -> 'a pattern -> ('a compiled_pattern, 'a error) result
  val compile_ : unit pattern -> (unit compiled_pattern, unit error) result
end
=
struct
  type 'a compiled_pattern = 'a -> path -> [ `NoMatch | `Matched of 'a M.t ]

  let singleton path attr = `Matched (M.singleton path attr)

  let join2_result ~join r1 r2 =
    match r1, r2 with
    | _, `NoMatch -> r1
    | `NoMatch, _ -> r2
    | `Matched ns1, `Matched ns2 ->
      let merger _ e1 e2 = Some (join e1 e2)
      in `Matched (M.union merger ns1 ns2)

  let join_result ~join rs = Seq.fold_left (join2_result ~join) `NoMatch rs

  let meet2_result ~meet r1 r2 =
    match r1, r2 with
    | _, `NoMatch | `NoMatch, _ -> `NoMatch
    | `Matched ns1, `Matched ns2 ->
      let merger _ a1 a2 =
        match a1, a2 with
        | None, _ | _, None -> None
        | Some a1, Some a2 -> Some (meet a1 a2)
      in `Matched (M.merge merger ns1 ns2)

  let meet_result ~meet rs =
    match rs () with
    | Seq.Nil -> invalid_arg "meet: empty list"
    | Seq.Cons (r, rs) -> Seq.fold_left (meet2_result ~meet) r rs

  let rec trim_prefix prefix path =
    match prefix, path with
    | [], _ -> Some path
    | _, [] -> None
    | (id :: prefix), (id' :: path) ->
      if id = id' then trim_prefix prefix path else None

  let rec compile ~join ~meet : 'a pattern -> ('a compiled_pattern, 'a error) result =
    function
    | PatWildcard ->
      Ok begin fun default ->
        function
        | [] -> `NoMatch
        | _::_ as path -> singleton path default
      end
    | PatScope (prefix, prefix_replacement, pattern) ->
      compile ~join ~meet pattern |> Result.map @@ fun m ->
      let prefix_replacement = Option.value prefix_replacement ~default:prefix in
      begin fun default path ->
        match trim_prefix prefix path with
        | None -> `NoMatch
        | Some remaining ->
          match m default remaining with
          | `NoMatch -> `NoMatch
          | `Matched ns ->
            `Matched (M.to_seq ns |> Seq.map (fun (r, attr) -> prefix_replacement @ r, attr) |> M.of_seq)
      end
    | PatSeq pats ->
      ResultMonad.map (compile ~join ~meet) pats |> Result.map @@ fun matchers ->
      let f r m default path =
        match r default path with
        | `NoMatch -> m default path
        | `Matched ns ->
          join_result ~join begin
            M.to_seq ns |> Seq.map @@ fun (path, default) ->
            match m default path with
            | `NoMatch -> singleton path default
            | `Matched ns -> `Matched ns
          end
      in
      List.fold_left ~f ~init:(fun _ _ -> `NoMatch) matchers
    | PatInv pat -> compile_inv ~join ~meet pat
    | PatAttr (default, pat) ->
      compile ~join ~meet pat |> Result.map @@ fun m _ -> m default
    | PatJoin pats ->
      ResultMonad.map (compile ~join ~meet) pats |> Result.map @@ fun ms default path ->
      join_result ~join begin List.to_seq ms |> Seq.map @@ fun m -> m default path end

  and compile_inv ~join ~meet : 'a pattern -> ('a compiled_pattern, 'a error) result =
    function
    | PatWildcard ->
      Ok begin fun default ->
        function
        | [] -> singleton [] default
        | _::_ -> `NoMatch
      end
    | PatScope (_, Some _, _) as pattern ->
      Error (ReplacementNotUsed pattern)
    | PatScope (prefix, None, pattern) ->
      compile_inv ~join ~meet pattern |> Result.map @@ fun m ->
      begin fun default path ->
        match trim_prefix prefix path with
        | None -> singleton path default
        | Some remaining ->
          match m default remaining with
          | `NoMatch -> `NoMatch
          | `Matched ns ->
            `Matched (M.to_seq ns |> Seq.map (fun (r, attr) -> prefix @ r, attr) |> M.of_seq)
      end
    | PatSeq pats ->
      ResultMonad.map (compile_inv ~join ~meet) pats |> Result.map @@ fun matchers ->
      let f r m default path =
        match r default path with
        | `NoMatch -> `NoMatch
        | `Matched ns ->
          join_result ~join begin
            M.to_seq ns |> Seq.map @@ fun (path, default) ->
            m default path
          end
      in
      List.fold_left ~f ~init:(fun default path -> singleton path default) matchers
    | PatInv pat -> compile ~join ~meet pat
    | PatAttr (default, pat) ->
      compile_inv ~join ~meet pat |> Result.map @@ fun m _ -> m default
    | PatJoin [] as pattern ->
      Error (EmptyMeet pattern)
    | PatJoin pats ->
      ResultMonad.map (compile_inv ~join ~meet) pats |> Result.map @@ fun ms default path ->
      meet_result ~meet begin List.to_seq ms |> Seq.map @@ fun m -> m default path end

  let compile_ : unit pattern -> (unit compiled_pattern, unit error) result =
    compile ~join:(fun _ _ -> ()) ~meet:(fun _ _ -> ())
end

include Compile

type 'a matching_result = [ `NoMatch | `Matched of (path * 'a) list ]
[@@deriving show]

let run m ~default path : 'a matching_result =
  match m default path with
  | `NoMatch -> `NoMatch
  | `Matched ns -> `Matched (M.bindings ns)

let run_ m path : unit matching_result =
  run m ~default:() path
