open StdLabels
open Bwd

module P = Pattern
module T = Trie

open ResultMonad.Syntax

type error = BindingNotFound
let error (p : string bwd) (e : error) = fail (p >> [], e)

let run_act p act t =
  match act with
  | P.ActFilterMap f -> ret @@ T.filter_map_endo f t
  | P.ActCheckExistence {if_existing; if_absent} ->
    if T.is_empty t then
      match if_absent with
      | `Ok -> ret t
      | `Error -> error p BindingNotFound
    else
      match if_existing with
      | `Keep -> ret t
      | `Hide -> ret T.empty

let rec run m p pat t =
  match pat with
  | P.PatAct act -> run_act p act t
  | P.PatSingletonSplit {path; path_replacement; on_singleton; on_others} ->
    let path_replacement = Option.value ~default:path path_replacement in
    let singleton, others = T.detach_singleton path t in
    let+ singleton = run m (p << path) on_singleton (T.mk_root singleton)
    and+ others = run m p on_others others in
    T.union_subtree m others (path_replacement, singleton)
  | P.PatSubtreeSplit {prefix; prefix_replacement; on_subtree; on_others} ->
    let prefix_replacement = Option.value ~default:prefix prefix_replacement in
    let subtree, others = T.detach_subtree prefix t in
    let+ subtree = run m (p << prefix) on_subtree subtree
    and+ others = run m p on_others others in
    T.union_subtree m others (prefix_replacement, subtree)
  | PatSeq pats ->
    let f t pat = Result.bind t (run m p pat) in
    List.fold_left ~f ~init:(ret t) pats
  | PatUnion pats ->
    let+ ts = ResultMonad.map (fun pat -> run m p pat t) pats in
    List.fold_left ~f:(T.union m) ~init:T.empty ts
