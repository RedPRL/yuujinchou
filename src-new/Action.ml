open StdLabels
module P = Pattern
module T = Trie

let run_act act t =
  match act with
  | P.ActSimple {if_existing; if_absent} ->
    begin
      if T.is_empty t then
        match if_absent with
        | `Error e -> Result.error ([], e)
        | `Ignored -> Result.ok t
      else
        match if_existing with
        | `Error e -> Result.error ([], e)
        | `Keep -> Result.ok t
        | `Hide -> Result.ok T.empty
    end
  | P.ActMap f -> Result.ok (T.map f t)
  | P.ActFilterMap f ->
    let f x =
      match f x with
      | `Error e -> Error e
      | `Keep x -> Ok (Some x)
      | `Hide -> Ok None
    in
    T.filter_map_err f t

let rec run m pat t =
  match pat with
  | P.PatAct act -> run_act act t
  | P.PatRootSplit {on_root; on_children} ->
    let root, children = T.detach_root t in
    Result.bind (run m on_root @@ T.mk_root root) @@ fun root ->
    Result.bind (run m on_children children) @@ fun children ->
    Ok (T.union m root children)
  | P.PatScopeSplit {prefix; prefix_replacement; on_subtree; on_others} ->
    let prefix_replacement = Option.value ~default:prefix prefix_replacement
    and subtree, others = T.detach_subtree prefix t
    in
    begin
      match run m on_subtree subtree with
      | Error (p, e) -> Error (prefix @ p, e)
      | Ok subtree ->
        Result.bind (run m on_others others) @@ fun others ->
        Ok (T.union_subtree m others (prefix_replacement, subtree))
    end
  | PatSeq pats ->
    let f t pat = Result.bind t @@ run m pat in
    List.fold_left ~f ~init:(Ok t) pats
  | PatUnion pats ->
    let f u pat =
      Result.bind u @@ fun u ->
      Result.bind (run m pat t) @@ fun t' ->
      Ok (T.union m u t')
    in
    List.fold_left ~f ~init:(Ok T.empty) pats
  | PatTry (pat1, pat2) ->
    match run m pat1 t with
    | Ok t -> Ok t
    | Error (_, e) -> run m (pat2 e) t
