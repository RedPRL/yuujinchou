open StdLabels
open Eff.StdlibShim

open Bwd
open BwdNotation

open Pattern

type bwd_path = Trie.bwd_path

module type Param =
sig
  type data
  type hook
end

module type S =
sig
  include Param

  type _ Effect.t +=
    | BindingNotFound : bwd_path -> unit Effect.t
    | Shadowing : bwd_path * data * data -> data Effect.t
    | Hook : hook * bwd_path * data Trie.t -> data Trie.t Effect.t

  val run : ?prefix:Trie.bwd_path -> hook Pattern.t -> data Trie.t -> data Trie.t
end

module Make (P : Param) =
struct
  include P

  type _ Effect.t +=
    | BindingNotFound : bwd_path -> unit Effect.t
    | Shadowing : bwd_path * data * data -> data Effect.t
    | Hook : hook * bwd_path * data Trie.t -> data Trie.t Effect.t

  let check_nonempty ~path t =
    if Trie.is_empty t then
      Effect.perform @@ BindingNotFound path

  let merger ~path x y = Effect.perform @@ Shadowing (path, x, y)

  let rec run_ ~prefix pat t =
    match pat with
    | P_only p ->
      let t = Trie.find_subtree p t in
      check_nonempty ~path:(prefix <>< p) t;
      Trie.prefix p t
    | P_except p ->
      let t, remaining = Trie.detach_subtree p t in
      check_nonempty ~path:(prefix <>< p) t;
      remaining
    | P_in (p, pat) ->
      Trie.update_subtree p (run_ ~prefix:(prefix <>< p) pat) t
    | P_renaming (p1, p2) ->
      let t, remaining = Trie.detach_subtree p1 t in
      check_nonempty ~path:(prefix <>< p1) t;
      Trie.union_subtree ~prefix merger remaining (p2, t)
    | P_seq pats ->
      let f t pat = run_ ~prefix pat t in
      List.fold_left ~f ~init:t pats
    | P_union pats ->
      let f ts pat =
        let ti = run_ ~prefix pat t in
        Trie.union ~prefix merger ts ti
      in
      List.fold_left ~f ~init:Trie.empty pats
    | P_hook h -> Effect.perform @@ Hook (h, prefix, t)

  let run ?(prefix=Emp) pat t = run_ ~prefix pat t
end
