open Pattern

type 'a error =
  | ReplacementNotUsed of 'a pattern
  | EmptyInverseJoin of 'a pattern
[@@deriving show]

module M = Map.Make (struct type t = path let compare = compare end)

let singleton path attr = `Matched (M.singleton path attr)

let join2_result ~join r1 r2 =
  match r1, r2 with
  | _, `NoMatch -> r1
  | `NoMatch, _ -> r2
  | `Matched m1, `Matched m2 ->
    let merger _ e1 e2 = Some (join e1 e2)
    in `Matched (M.union merger m1 m2)

let join_result ~join rs = List.fold_left (join2_result ~join) `NoMatch rs

let meet2_result ~meet r1 r2 =
  match r1, r2 with
  | _, `NoMatch | `NoMatch, _ -> `NoMatch
  | `Matched m1, `Matched m2 ->
    let merger _ a1 a2 =
      match a1, a2 with
      | None, _ | _, None -> None
      | Some a1, Some a2 -> Some (meet a1 a2)
    in `Matched (M.merge merger m1 m2)

let meet_result ~meet =
  function
  | [] -> invalid_arg "meet: empty list"
  | r :: rs -> List.fold_left (meet2_result ~meet) r rs

type mode = [`Normal | `Inverse]

let flip_mode : mode -> mode =
  function
  | `Normal -> `Inverse
  | `Inverse -> `Normal

let rec trim_prefix prefix path =
  match prefix, path with
  | [], _ -> Some path
  | _, [] -> None
  | (id :: prefix), (id' :: path) ->
    if id = id' then trim_prefix prefix path else None

type 'a modal_result = [ `NoMatch | `Matched of 'a M.t ]

let rec modal_run ~mode ~default ~join ~meet pattern path : ('a modal_result, 'a error) result =
  match mode, pattern, path with
  | `Normal, PatWildcard, (_ :: _) | `Inverse, PatWildcard, [] ->
    Ok (singleton path default)
  | `Inverse, PatWildcard, (_ :: _) | `Normal, PatWildcard, [] ->
    Ok `NoMatch
  | `Inverse, PatScope (_, Some _, _), _ ->
    Error (ReplacementNotUsed pattern)
  | _, PatScope (prefix, prefix_replacement, pattern), _ ->
    begin
      match trim_prefix prefix path with
      | None ->
        begin
          match mode with
          | `Normal -> Ok `NoMatch
          | `Inverse -> Ok (singleton path default)
        end
      | Some remaining ->
        let prefix_replacement = Option.value prefix_replacement ~default:prefix in
        modal_run ~mode ~default ~join ~meet pattern remaining |> Result.map begin
          function
          | `NoMatch -> `NoMatch
          | `Matched m ->
            `Matched (m |> M.to_seq |> Seq.map (fun (r, attr) -> prefix_replacement @ r, attr) |> M.of_seq)
        end
    end
  | `Normal, PatSeq pats, _ ->
    let f r pat = Result.bind r @@
      function
      | `NoMatch -> modal_run ~mode ~default ~join ~meet pat path
      | `Matched m ->
        Result.map (join_result ~join) begin
          M.bindings m |> ResultMonad.map @@ fun (path, default) ->
          modal_run ~mode ~default ~join ~meet pat path
        end
    in
    List.fold_left f (Ok `NoMatch) pats
  | `Inverse, PatSeq pats, _ ->
    let f r pat = Result.bind r @@
      function
      | `NoMatch -> Ok `NoMatch
      | `Matched m ->
        Result.map (join_result ~join) begin
          M.bindings m |> ResultMonad.map @@ fun (path, default) ->
          modal_run ~mode ~default ~join ~meet pat path
        end
    in
    List.fold_left f (Ok (singleton path default)) pats
  | _, PatInv pat, _ -> modal_run ~mode:(flip_mode mode) ~default ~join ~meet pat path
  | _, PatAttr (default, pat), _ -> modal_run ~mode ~default ~join ~meet pat path
  | `Normal, PatJoin pats, _ ->
    Result.map (join_result ~join) begin
      pats |> ResultMonad.map @@ fun pat -> modal_run ~mode ~default ~join ~meet pat path
    end
  | `Inverse, PatJoin [], _ ->
    Error (EmptyInverseJoin pattern)
  | `Inverse, PatJoin pats, _ ->
    Result.map (meet_result ~meet) begin
      pats |> ResultMonad.map @@ fun pat -> modal_run ~mode ~default ~join ~meet pat path
    end

type 'a result_ = [ `NoMatch | `Matched of (path * 'a) list ]
[@@deriving show]

let pp_result pp_a = Format.pp_print_result ~ok:(pp_result_ pp_a) ~error:(pp_error pp_a)

let run ~default ~join ~meet pattern path : ('a result_, 'a error) result =
  modal_run ~mode:`Normal ~default ~join ~meet pattern path |> Result.map @@
  function
  | `NoMatch -> `NoMatch
  | `Matched m -> `Matched (M.bindings m)

let rec modal_check ~mode pattern : (unit, 'a error) result =
  match mode, pattern with
  | _, PatWildcard -> Ok ()
  | `Inverse, PatScope (_, Some _, _) -> Error (ReplacementNotUsed pattern)
  | _, PatScope (_, _, pattern) -> modal_check ~mode pattern
  | _, PatSeq pats -> ResultMonad.iter (modal_check ~mode) pats
  | _, PatInv pattern -> modal_check ~mode:(flip_mode mode) pattern
  | _, PatAttr (_, pattern) -> modal_check ~mode pattern
  | `Inverse, PatJoin [] ->
    Error (EmptyInverseJoin pattern)
  | _, PatJoin pats ->
    ResultMonad.iter (modal_check ~mode) pats

let check pattern : (unit, 'a error) result =
  modal_check ~mode:`Normal pattern

let pp_check_result pp_a =
  Format.pp_print_result ~ok:(fun fmt () -> Format.pp_print_string fmt "()") ~error:(pp_error pp_a)
