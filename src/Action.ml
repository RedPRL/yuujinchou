open StdLabels
open Bwd
open Pattern
open ResultMonad.Syntax

type path = Pattern.path

type error = BindingNotFound of path

let run_act p act t =
  match act with
  | ActFilterMap f -> ret @@ Trie.filter_map_endo f t
  | ActSwitch switch ->
    if Trie.is_empty t then
      fail (BindingNotFound (p >> []))
    else
      match switch with
      | `Keep -> ret t
      | `Hide -> ret Trie.empty

let rec run_ m p pat t =
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
    let+ target' = run_ m (p << prefix) on_target target
    and+ others' = run_ m p on_others others in
    if target == target' && others == others' && prefix = prefix_replacement
    then t else Trie.union_subtree m others' (prefix_replacement, target')
  | PatSeq pats ->
    let f t pat = Result.bind t (run_ m p pat) in
    List.fold_left ~f ~init:(ret t) pats
  | PatUnion pats ->
    let+ ts = ResultMonad.map (fun pat -> run_ m p pat t) pats in
    List.fold_left ~f:(Trie.union m) ~init:Trie.empty ts

let run m = run_ m Nil

let pp_error fmt =
  function
  | BindingNotFound path -> Format.fprintf fmt "@[<hov 1>(not-found@ %s)@]" (String.concat ~sep:"." path)
