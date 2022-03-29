open StdLabels
open Pattern

type path = Pattern.path

module type S =
sig
  type data
  type hook
  type _ Effect.t +=
    | BindingNotFound : path -> unit Effect.t
    | Shadowing : path * data * data -> data Effect.t
    | Hook : hook * path * data Trie.t -> data Trie.t Effect.t

  val run : ?rev_prefix:Pattern.path -> hook Pattern.t -> data Trie.t -> data Trie.t
end

module Make (Param : sig type data type hook end) =
struct
  include Param

  type _ Effect.t +=
    | BindingNotFound : path -> unit Effect.t
    | Shadowing : path * data * data -> data Effect.t
    | Hook : hook * path * data Trie.t -> data Trie.t Effect.t

  let check_nonempty ~rev_path t =
    if Trie.is_empty t then
      Effect.perform @@ BindingNotFound rev_path

  let merger ~rev_path x y = Effect.perform @@ Shadowing (rev_path, x, y)

  let rec run_ ~rev_prefix pat t =
    match pat with
    | P_only p ->
      let t = Trie.find_subtree p t in
      check_nonempty ~rev_path:(List.rev_append p rev_prefix) t;
      Trie.prefix p t
    | P_except p ->
      let t, remaining = Trie.detach_subtree p t in
      check_nonempty ~rev_path:(List.rev_append p rev_prefix) t;
      remaining
    | P_in (p, pat) ->
      Trie.update_subtree p (run_ ~rev_prefix:(List.rev_append p rev_prefix) pat) t
    | P_renaming (p1, p2) ->
      let t, remaining = Trie.detach_subtree p1 t in
      check_nonempty ~rev_path:(List.rev_append p1 rev_prefix) t;
      Trie.union_subtree ~rev_prefix merger remaining (p2, t)
    | P_seq pats ->
      let f t pat = run_ ~rev_prefix pat t in
      List.fold_left ~f ~init:t pats
    | P_union pats ->
      let f ts pat =
        let ti = run_ ~rev_prefix pat t in
        Trie.union ~rev_prefix merger ts ti
      in
      List.fold_left ~f ~init:Trie.empty pats
    | P_hook h -> Effect.perform @@ Hook (h, rev_prefix, t)

  let run ?(rev_prefix=[]) pat t = run_ ~rev_prefix pat t
end
