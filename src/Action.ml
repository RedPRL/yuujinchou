open Pattern

type error =
  | ReplacementNotUsed of path * path
  | EmptyMeet of pattern

module M = Map.Make (struct type t = path let compare = compare end)

type result = NoMatch | Matched of exportability M.t

let singleton path export = Matched (M.singleton path export)

let join2 r1 r2 =
  match r1, r2 with
  | _, NoMatch -> r1
  | NoMatch, _ -> r2
  | Matched m1, Matched m2 ->
    let merger _ e1 e2 =
      match e1, e2 with
      | `Public, _ | _, `Public -> Some `Public
      | `Private, `Private -> Some `Private
    in Matched (M.union merger m1 m2)

let join rs = List.fold_left join2 NoMatch rs

type inversion = [`Normal | `Negated]
let flip_inversion =
  function
  | `Normal -> `Negated
  | `Negated -> `Normal

let rec trim_prefix prefix path =
  match prefix, path with
  | [], _ -> Some path
  | _, [] -> None
  | (id :: prefix), (id' :: path) ->
    if id = id' then Option.map (fun l -> id :: l) (trim_prefix prefix path) else None

let rec run ~inversion ~export pattern path =
  match inversion, pattern, path with
  | `Normal, PatWildcard, [] -> Ok NoMatch
  | `Negated, PatWildcard, [] -> Ok (singleton [] export)
  | `Normal, PatWildcard, (_ :: _) -> Ok (singleton path export)
  | `Negated, PatWildcard, (_ :: _) -> Ok NoMatch
  | `Negated, PatScope (prefix, Some prefix_replacement, _), _ ->
    Error (ReplacementNotUsed (prefix, prefix_replacement))
  | _, PatScope (prefix, prefix_replacement, pattern), path ->
    begin
      match trim_prefix prefix path with
      | None ->
        begin
          match inversion with
          | `Normal -> Ok NoMatch
          | `Negated -> Ok (singleton path export)
        end
      | Some remaining ->
        let prefix_replacement = Option.value prefix_replacement ~default:prefix in
        run ~inversion ~export pattern remaining |> Result.map begin
          function
          | NoMatch -> NoMatch
          | Matched m ->
            Matched (m |> M.to_seq |> Seq.map (fun (r, export) -> prefix_replacement @ r, export) |> M.of_seq)
        end
    end
  | `Normal, PatId (prefix, prefix_replacement), path ->
    let prefix_replacement = Option.value prefix_replacement ~default:prefix in
    begin
      match trim_prefix prefix path with
      | None -> Ok NoMatch
      | Some remaining -> Ok (singleton (prefix_replacement @ remaining) export)
    end
  | `Negated, PatId (prefix, Some prefix_replacement), _ ->
    Error (ReplacementNotUsed (prefix, prefix_replacement))
  | `Negated, PatId (prefix, None), path ->
    begin
      match trim_prefix prefix path with
      | None -> Ok (singleton path export)
      | Some _ -> Ok NoMatch
    end
  | `Normal, PatSeq acts, path ->
    let f r act = Result.bind r @@
      function
      | NoMatch -> run ~inversion ~export act path
      | Matched m ->
        Result.map join begin
          M.bindings m |> ResultMonad.map @@ fun (path, export) ->
          run ~inversion ~export act path
        end
    in
    List.fold_left f (Ok NoMatch) acts
  | `Negated, PatSeq acts, path ->
    let f r act = Result.bind r @@
      function
      | NoMatch -> Ok NoMatch
      | Matched m ->
        Result.map join begin
          M.bindings m |> ResultMonad.map @@ fun (path, export) ->
          run ~inversion ~export act path
        end
    in
    List.fold_left f (Ok (singleton path export)) acts
  | _, PatNeg act, path -> run ~inversion:(flip_inversion inversion) ~export act path
  | _, PatExport (export, act), path -> run ~inversion ~export act path
