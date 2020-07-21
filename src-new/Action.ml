open StdLabels
module P = Pattern
module T = Trie

type 'a bwd = Nil | Snoc of 'a bwd * 'a

let rec (<<) xs ys =
  match ys with
  | [] -> xs
  | y :: ys -> Snoc (xs, y) << ys

let rec (>>) xs ys =
  match xs with
  | Nil -> ys
  | Snoc (xs, x) -> xs >> x :: ys

open ResultMonad.Syntax

type error = DefinitionNotFound
let error (p : string bwd) (e : error) = fail (p >> [], e)

let run_act p act t =
  match act with
  | P.ActFilterMap f -> ret @@ T.filter_map_endo f t
  | P.ActCheckExistence {if_existing; if_absent} ->
    if T.is_empty t then
      match if_absent with
      | `Error -> error p DefinitionNotFound
      | `Ignored -> ret t
    else
      match if_existing with
      | `Error -> error p DefinitionNotFound
      | `Keep -> ret t
      | `Hide -> ret T.empty

let rec run m p pat t =
  match pat with
  | P.PatAct act -> run_act p act t
  | P.PatRootSplit {on_root; on_children} ->
    let root, children = T.detach_root t in
    let+ root = run m p on_root @@ T.mk_root root
    and+ children = run m p on_children children in
    T.union m root children
  | P.PatScopeSplit {prefix; prefix_replacement; on_subtree; on_others} ->
    let prefix_replacement = Option.value ~default:prefix prefix_replacement in
    let subtree, others = T.detach_subtree prefix t in
    let+ subtree = run m (p << prefix) on_subtree subtree
    and+ others = run m p on_others others in
    T.union_subtree m others (prefix_replacement, subtree)
  | PatSeq pats ->
    let f t pat = let* t = t in run m p pat t in
    List.fold_left ~f ~init:(ret t) pats
  | PatUnion pats ->
    let f u pat =
      let+ u = u and+ t = run m p pat t in
      T.union m u t
    in
    List.fold_left ~f ~init:(ret T.empty) pats
