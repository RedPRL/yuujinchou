open StdLabels
open Pattern
open ResultMonad.Syntax

type path = Pattern.path

let run_act ~custom ~rev_prefix act t =
  match act with
  | A_custom f -> custom f ~rev_prefix t
  | A_switch switch ->
    if Trie.is_empty t then
      error @@ `BindingNotFound (List.rev rev_prefix)
    else
      match switch with
      | `Keep -> ret t
      | `Hide -> ret Trie.empty

let rec run_ ~union ~custom ~rev_prefix pat t =
  match pat with
  | P_act act -> run_act ~custom ~rev_prefix act t
  | P_split {mode; prefix; prefix_replacement; on_target; on_others} ->
    let prefix_replacement = Option.value ~default:prefix prefix_replacement in
    let target, others =
      match mode with
      | `Subtree -> Trie.detach_subtree prefix t
      | `Node ->
        let singleton, others = Trie.detach_singleton prefix t in
        Trie.mk_root singleton, others
    in
    let+ target' = run_ ~union ~custom ~rev_prefix:List.(rev_append prefix rev_prefix) on_target target
    and+ others' = run_ ~union ~custom ~rev_prefix on_others others in
    (* test physical equality *)
    if target == target' && others == others' && prefix = prefix_replacement
    then t else Trie.union_subtree union others' (prefix_replacement, target')
  | P_seq pats ->
    let f t pat = t >>= run_ ~union ~custom ~rev_prefix pat in
    List.fold_left ~f ~init:(ret t) pats
  | P_union pats ->
    let+ ts = ResultMonad.map (fun pat -> run_ ~union ~custom ~rev_prefix pat t) pats in
    List.fold_left ~f:(Trie.union union) ~init:Trie.empty ts

let run_with_custom ?(rev_prefix=[]) ~union ~custom = run_ ~union ~custom ~rev_prefix

let run ?rev_prefix ~union = run_with_custom ?rev_prefix ~union ~custom:(fun () ~rev_prefix:_ t -> ret t)

let pp_path fmt path = Format.pp_print_string fmt @@ String.concat ~sep:"." path
