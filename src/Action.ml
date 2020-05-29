type inversion = [`Normal | `Negated]
let flip_inversion =
  function
  | `Normal -> `Negated
  | `Negated -> `Normal

type action =
  | ActWildcard
  | ActId of string list * string list
  | ActScope of string list * string list * action
  | ActSeq of action * action (* will be n-ary later *)
  | ActNeg of action

type result = NoMatch | Matched of string list

let rec check_prefix prefix path =
  match prefix, path with
  | [], _ -> Some path
  | _, [] -> None
  | (id :: prefix), (id' :: path) ->
    if id = id' then Option.map (fun l -> id :: l) (check_prefix prefix path) else None

let rec run ~inversion action path =
  match inversion, action, path with
  | `Normal, ActWildcard, [] -> NoMatch
  | `Negated, ActWildcard, [] -> Matched []
  | `Normal, ActWildcard, (_ :: _) -> Matched path
  | `Negated, ActWildcard, (_ :: _) -> NoMatch
  | _, ActScope (prefix, replacement, action), path ->
    begin
      match check_prefix prefix path with
      | None ->
        begin
          match inversion with
          | `Normal -> NoMatch
          | `Negated -> Matched path
        end
      | Some remaining ->
        begin
          match run ~inversion action remaining with
          | NoMatch -> NoMatch
          | Matched remaining_replacement -> Matched (replacement @ remaining_replacement)
        end
    end
  | _, ActId (prefix, replacement), path ->
    begin
      match check_prefix prefix path with
      | None ->
        begin
          match inversion with
          | `Normal -> NoMatch
          | `Negated -> Matched path
        end
      | Some remaining ->
        begin
          match inversion with
          | `Normal -> Matched (replacement @ remaining)
          | `Negated -> NoMatch (* should warn the user that the replacement not used *)
        end
    end
  | _, ActSeq (act1, act2), path ->
    begin
      match run ~inversion act1 path with
      | NoMatch ->
        begin
          match inversion with
          | `Normal -> run ~inversion act2 path
          | `Negated -> NoMatch
        end
      | Matched replacement1 ->
        match run ~inversion act2 replacement1 with
        | NoMatch ->
          begin
            match inversion with
            | `Normal -> Matched replacement1
            | `Negated -> NoMatch
          end
        | Matched replacement2 -> Matched replacement2
    end
  | _, ActNeg act, path -> run ~inversion:(flip_inversion inversion) act path
