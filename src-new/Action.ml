open StdLabels
module P = Pattern
module T = Trie

let run_act act t =
  match act with
  | P.ActCheckExistence {if_existing; if_absent} ->
    begin
      if T.is_empty t then
        match if_absent with
        | `Error e -> Error ([], e)
        | `Ignored -> Ok t
      else
        match if_existing with
        | `Error e -> Error ([], e)
        | `Keep -> Ok t
        | `Hide -> Ok T.empty
    end
  | P.ActFilterMap f -> Ok (T.filter_map_endo f t)

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
