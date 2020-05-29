type inversion = [`Normal | `Negated]

let flip_inversion =
  function
  | `Normal -> `Negated
  | `Negated -> `Normal

type path = string list

type error = ReplacementNotUsed of path * path

type exportability = [`Public | `Private]

type action =
  | ActWildcard
  | ActId of path * path option
  | ActScope of path * path option * action
  | ActSeq of action * action (* will be n-ary later *)
  | ActNeg of action
  | ActExport of exportability * action

type result = NoMatch | Matched of exportability * string list

let rec trim_prefix prefix path =
  match prefix, path with
  | [], _ -> Some path
  | _, [] -> None
  | (id :: prefix), (id' :: path) ->
    if id = id' then Option.map (fun l -> id :: l) (trim_prefix prefix path) else None

let rec run ~inversion ~export action path =
  match inversion, action, path with
  | `Normal, ActWildcard, [] -> Ok NoMatch
  | `Negated, ActWildcard, [] -> Ok (Matched (export, []))
  | `Normal, ActWildcard, (_ :: _) -> Ok (Matched (export, path))
  | `Negated, ActWildcard, (_ :: _) -> Ok NoMatch
  | `Negated, ActScope (prefix, Some prefix_replacement, _), _ ->
    Error (ReplacementNotUsed (prefix, prefix_replacement))
  | _, ActScope (prefix, prefix_replacement, action), path ->
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
        run ~inversion ~export action remaining |> Result.map begin
          function
          | NoMatch -> NoMatch
          | Matched (export, remaining_replacement) -> Matched (export, prefix_replacement @ remaining_replacement)
        end
    end
  | `Normal, ActId (prefix, prefix_replacement), path ->
    let prefix_replacement = Option.value prefix_replacement ~default:prefix in
    begin
      match trim_prefix prefix path with
      | None -> Ok NoMatch
      | Some remaining -> Ok (Matched (export, prefix_replacement @ remaining))
    end
  | `Negated, ActId (prefix, Some prefix_replacement), _ ->
    Error (ReplacementNotUsed (prefix, prefix_replacement))
  | `Negated, ActId (prefix, None), path ->
    begin
      match trim_prefix prefix path with
      | None -> Ok (Matched (export, path))
      | Some _ -> Ok NoMatch
    end
  | `Normal, ActSeq (act1, act2), path ->
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
  | `Negated, ActSeq (act1, act2), path ->
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
  | _, ActNeg act, path -> run ~inversion:(flip_inversion inversion) ~export act path
  | _, ActExport (export, act), path -> run ~inversion ~export act path
