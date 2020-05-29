open Pattern

type error = ReplacementNotUsed of path * path

type result = NoMatch | Matched of exportability * string list

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
  | `Negated, PatWildcard, [] -> Ok (Matched (export, []))
  | `Normal, PatWildcard, (_ :: _) -> Ok (Matched (export, path))
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
          | `Negated -> Ok (Matched (export, path))
        end
      | Some remaining ->
        let prefix_replacement = Option.value prefix_replacement ~default:prefix in
        run ~inversion ~export pattern remaining |> Result.map begin
          function
          | NoMatch -> NoMatch
          | Matched (export, remaining_replacement) -> Matched (export, prefix_replacement @ remaining_replacement)
        end
    end
  | `Normal, PatId (prefix, prefix_replacement), path ->
    let prefix_replacement = Option.value prefix_replacement ~default:prefix in
    begin
      match trim_prefix prefix path with
      | None -> Ok NoMatch
      | Some remaining -> Ok (Matched (export, prefix_replacement @ remaining))
    end
  | `Negated, PatId (prefix, Some prefix_replacement), _ ->
    Error (ReplacementNotUsed (prefix, prefix_replacement))
  | `Negated, PatId (prefix, None), path ->
    begin
      match trim_prefix prefix path with
      | None -> Ok (Matched (export, path))
      | Some _ -> Ok NoMatch
    end
  | `Normal, PatSeq (act1, act2), path ->
    begin
      Result.bind (run ~inversion ~export act1 path) @@
      function
      | NoMatch -> run ~inversion ~export act2 path
      | Matched (export, replacement1) ->
        Result.bind (run ~inversion ~export act2 replacement1) @@
        function
        | NoMatch -> Ok (Matched (export, replacement1))
        | Matched (export, replacement2) -> Ok (Matched (export, replacement2))
    end
  | `Negated, PatSeq (act1, act2), path ->
    begin
      Result.bind (run ~inversion ~export act1 path) @@
      function
      | NoMatch -> Ok NoMatch
      | Matched (export, replacement1) ->
        Result.bind (run ~inversion ~export act2 replacement1) @@
        function
        | NoMatch -> Ok NoMatch
        | Matched (export, replacement2) -> Ok (Matched (export, replacement2))
    end
  | _, PatNeg act, path -> run ~inversion:(flip_inversion inversion) ~export act path
  | _, PatExport (export, act), path -> run ~inversion ~export act path
