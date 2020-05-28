type action =
  | ActWildcard
  | ActId of string list
  | ActScope of string list * action
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
  | ActScope (prefix, action), path ->
    begin
      match check_prefix prefix path with
      | None -> NoMatch
      | Some remaining ->
        match run action remaining with
        | NoMatch -> NoMatch
        | Matched remaining -> Matched (prefix @ remaining)
    end
  | ActId prefix, path ->
    begin
      match check_prefix prefix path with
      | None -> NoMatch
      | Some _ -> Matched path
    end
  | ActSeq (act1, act2), path ->
    let path =
        match run act1 path with
        | NoMatch -> path
        | Matched path -> path
    in
    run act2 path

