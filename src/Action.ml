open StdLabels
open Pattern

type path = Pattern.path

type _ Effect.t += BindingNotFound : path -> unit Effect.t

let check_nonempty ~path t =
  if Trie.is_empty t then
    Effect.perform @@ BindingNotFound path

let rec run_ ~union ~hooks ~rev_prefix pat t =
  match pat with
  | P_only p ->
    let t = Trie.find_subtree p t in
    check_nonempty ~path:(List.rev_append rev_prefix p) t;
    Trie.prefix p t
  | P_except p ->
    let t, remaining = Trie.detach_subtree p t in
    check_nonempty ~path:(List.rev_append rev_prefix p) t;
    remaining
  | P_in (p, pat) ->
    Trie.update_subtree p (run_ ~union ~hooks ~rev_prefix:(List.rev_append p rev_prefix) pat) t
  | P_renaming (p1, p2) ->
    let t, remaining = Trie.detach_subtree p1 t in
    check_nonempty ~path:(List.rev_append rev_prefix p1) t;
    Trie.union_subtree ~rev_prefix union remaining (p2, t)
  | P_seq pats ->
    let f t pat = run_ ~union ~hooks ~rev_prefix pat t in
    List.fold_left ~f ~init:t pats
  | P_union pats ->
    let f ts pat =
      let ti = run_ ~union ~hooks ~rev_prefix pat t in
      Trie.union ~rev_prefix union ts ti
    in
    List.fold_left ~f ~init:Trie.empty pats
  | P_hook h -> hooks h ~rev_prefix t

let run_with_hooks ?(rev_prefix=[]) ~union ~hooks = run_ ~union ~hooks ~rev_prefix

let run ?rev_prefix ~union = run_with_hooks ?rev_prefix ~union ~hooks:(fun () ~rev_prefix:_ t -> t)
