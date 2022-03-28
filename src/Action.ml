open StdLabels
open Pattern

type path = Pattern.path

type _ Effect.t += BindingNotFound : path -> unit Effect.t

let check_nonempty ~path t =
  if Trie.is_empty t then
    Effect.perform @@ BindingNotFound path

let rec run_ ~merger ~hooks ~rev_prefix pat t =
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
    Trie.update_subtree p (run_ ~merger ~hooks ~rev_prefix:(List.rev_append p rev_prefix) pat) t
  | P_renaming (p1, p2) ->
    let t, remaining = Trie.detach_subtree p1 t in
    check_nonempty ~path:(List.rev_append rev_prefix p1) t;
    Trie.union_subtree ~rev_prefix merger remaining (p2, t)
  | P_seq pats ->
    let f t pat = run_ ~merger ~hooks ~rev_prefix pat t in
    List.fold_left ~f ~init:t pats
  | P_union pats ->
    let f ts pat =
      let ti = run_ ~merger ~hooks ~rev_prefix pat t in
      Trie.union ~rev_prefix merger ts ti
    in
    List.fold_left ~f ~init:Trie.empty pats
  | P_hook h -> hooks h ~rev_prefix t

let run_with_hooks ?(rev_prefix=[]) ~merger ~hooks pat t =
  run_ ~merger ~hooks ~rev_prefix pat t

let run ?rev_prefix ~merger pat t =
  run_with_hooks ?rev_prefix ~merger ~hooks:(fun () ~rev_prefix:_ t -> t) pat t
