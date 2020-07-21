open StdLabels
open Bwd
open Pattern
open ResultMonad.Syntax

type error = BindingNotFound
let error (p : string bwd) (e : error) = fail (p >> [], e)

let run_act p act t =
  match act with
  | ActFilterMap f -> ret @@ Trie.filter_map_endo f t
  | ActCheckExistence {if_existing; if_absent} ->
    if Trie.is_empty t then
      match if_absent with
      | `Ok -> ret t
      | `Error -> error p BindingNotFound
    else
      match if_existing with
      | `Keep -> ret t
      | `Hide -> ret Trie.empty

let rec run m p pat t =
  match pat with
  | PatAct act -> run_act p act t
  | PatSplit {mode; prefix; prefix_replacement; on_target; on_others} ->
    let prefix_replacement = Option.value ~default:prefix prefix_replacement in
    let target, others =
      match mode with
      | `Subtree -> Trie.detach_subtree prefix t
      | `Node ->
        let singleton, others = Trie.detach_singleton prefix t in
        Trie.mk_root singleton, others
    in
    let+ target = run m (p << prefix) on_target target
    and+ others = run m p on_others others in
    Trie.union_subtree m others (prefix_replacement, target)
  | PatSeq pats ->
    let f t pat = Result.bind t (run m p pat) in
    List.fold_left ~f ~init:(ret t) pats
  | PatUnion pats ->
    let+ ts = ResultMonad.map (fun pat -> run m p pat t) pats in
    List.fold_left ~f:(Trie.union m) ~init:Trie.empty ts
