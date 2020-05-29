type inversion = [`Normal | `Negated]

let flip_inversion =
  function
  | `Normal -> `Negated
  | `Negated -> `Normal

type exportability = [`Public | `Private]

type action =
  | ActWildcard
  | ActId of string list * string list
  | ActScope of string list * string list * action
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
  | `Normal, ActWildcard, [] -> NoMatch
  | `Negated, ActWildcard, [] -> Matched (export, [])
  | `Normal, ActWildcard, (_ :: _) -> Matched (export, path)
  | `Negated, ActWildcard, (_ :: _) -> NoMatch
  | _, ActScope (prefix, replacement, action), path ->
    begin
      match trim_prefix prefix path with
      | None ->
        begin
          match inversion with
          | `Normal -> NoMatch
          | `Negated -> Matched (export, path)
        end
      | Some remaining ->
        begin
          match run ~inversion ~export action remaining with
          | NoMatch -> NoMatch
          | Matched (export, remaining_replacement) -> Matched (export, replacement @ remaining_replacement)
        end
    end
  | `Normal, ActId (prefix, replacement), path ->
    begin
      match trim_prefix prefix path with
      | None -> NoMatch
      | Some remaining -> Matched (export, replacement @ remaining)
    end
  | `Negated, ActId (prefix, _), path ->
    (* TODO should warn user if replacement != prefix *)
    begin
      match trim_prefix prefix path with
      | None -> Matched (export, path)
      | Some _ -> NoMatch
    end
  | `Normal, ActSeq (act1, act2), path ->
    begin
      match run ~inversion ~export act1 path with
      | NoMatch -> run ~inversion ~export act2 path
      | Matched (export, replacement1) ->
        match run ~inversion ~export act2 replacement1 with
        | NoMatch -> Matched (export, replacement1)
        | Matched (export, replacement2) -> Matched (export, replacement2)
    end
  | `Negated, ActSeq (act1, act2), path ->
    begin
      match run ~inversion ~export act1 path with
      | NoMatch -> NoMatch
      | Matched (export, replacement1) ->
        match run ~inversion ~export act2 replacement1 with
        | NoMatch -> NoMatch
        | Matched (export, replacement2) -> Matched (export, replacement2)
    end
  | _, ActNeg act, path -> run ~inversion:(flip_inversion inversion) ~export act path
  | _, ActExport (export, act), path -> run ~inversion ~export act path
