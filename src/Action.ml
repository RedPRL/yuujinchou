open StdLabels
open Pattern
open ResultMonad.Syntax

type path = Pattern.path
let pp_path = Pattern.pp_path

type nonrec ('a, 'error) result = ('a Trie.t, [> `BindingNotFound of path] as 'error) result

let check_nonempty ~path t =
  if Trie.is_empty t then
    error @@ `BindingNotFound path
  else
    ret ()

let rec run_ ~union ~hooks ~rev_prefix pat t =
  match pat with
  | P_only p ->
    let t = Trie.find_subtree p t in
    let+ () = check_nonempty ~path:(List.rev_append rev_prefix p) t in
    Trie.prefix p t
  | P_except p ->
    let t, remaining = Trie.detach_subtree p t in
    let+ () = check_nonempty ~path:(List.rev_append rev_prefix p) t in
    remaining
  | P_in (p, pat) ->
    Trie.Result.update_subtree p (run_ ~union ~hooks ~rev_prefix:(List.rev_append p rev_prefix) pat) t
  | P_renaming (p1, p2) ->
    let t, remaining = Trie.detach_subtree p1 t in
    let+ () = check_nonempty ~path:(List.rev_append rev_prefix p1) t in
    Trie.union_subtree ~rev_prefix union remaining (p2, t)
  | P_seq pats ->
    let f t pat = t >>= run_ ~union ~hooks ~rev_prefix pat in
    List.fold_left ~f ~init:(ret t) pats
  | P_union pats ->
    let+ ts = ResultMonad.map (fun pat -> run_ ~union ~hooks ~rev_prefix pat t) pats in
    List.fold_left ~f:(Trie.union union) ~init:Trie.empty ts
  | P_hook h -> hooks h ~rev_prefix t

let run_with_hooks ?(rev_prefix=[]) ~union ~hooks = run_ ~union ~hooks ~rev_prefix

let run ?rev_prefix ~union = run_with_hooks ?rev_prefix ~union ~hooks:(fun () ~rev_prefix:_ t -> ret t)
