type action =
  | ActWildcard
  | ActId of string list * string list
  | ActScope of string list * string list * action
  | ActSeq of action * action (* will be n-ary later *)

type result = NoMatch | Matched of string list

let rec check_prefix prefix path =
  match prefix, path with
  | [], _ -> Some path
  | _, [] -> None
  | (id :: prefix), (id' :: path) ->
    if id = id' then Option.map (fun l -> id :: l) (check_prefix prefix path) else None

let rec run action path =
  match action, path with
  | ActWildcard, [] -> NoMatch
  | ActWildcard, (_ :: _) -> Matched path
  | ActScope (prefix, replacement, action), path ->
    begin
      match check_prefix prefix path with
      | None -> NoMatch
      | Some remaining ->
        match run action remaining with
        | NoMatch -> NoMatch
        | Matched remaining_replacement -> Matched (replacement @ remaining_replacement)
    end
  | ActId (prefix, replacement), path ->
    begin
      match check_prefix prefix path with
      | None -> NoMatch
      | Some remaining -> Matched (replacement @ remaining)
    end
  | ActSeq (act1, act2), path ->
    begin
      match run act1 path with
      | NoMatch -> run act2 path
      | Matched replacement1 ->
        match run act2 replacement1 with
        | NoMatch -> Matched replacement1
        | Matched replacement2 -> Matched replacement2
    end
