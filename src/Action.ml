open StdLabels
open Algaeff.StdlibShim

open Bwd
open BwdNotation

open Modifier

module type Param =
sig
  type data
  type hook
end

module type S =
sig
  include Param

  type source = ..

  type _ Effect.t +=
    | BindingNotFound : source option * Trie.bwd_path -> unit Effect.t
    | Shadowing : source option * Trie.bwd_path * data * data -> data Effect.t
    | Hook : source option * Trie.bwd_path * hook * data Trie.t -> data Trie.t Effect.t

  val run : ?source:source -> ?prefix:Trie.bwd_path -> hook Modifier.t -> data Trie.t -> data Trie.t
end

module Make (P : Param) =
struct
  include P

  type source = ..

  type _ Effect.t +=
    | BindingNotFound : source option * Trie.bwd_path -> unit Effect.t
    | Shadowing : source option * Trie.bwd_path * data * data -> data Effect.t
    | Hook : source option * Trie.bwd_path * hook * data Trie.t -> data Trie.t Effect.t

  let run_ ~source =
    let check_nonempty ~path t =
      if Trie.is_empty t then
        Effect.perform @@ BindingNotFound (source, path)
    in
    let merger ~path x y = Effect.perform @@ Shadowing (source, path, x, y) in
    let hook h ~prefix t = Effect.perform @@ Hook (source, prefix, h, t) in
    let rec go ~prefix pat t =
      match pat with
      | M_only p ->
        let t = Trie.find_subtree p t in
        check_nonempty ~path:(prefix <>< p) t;
        Trie.prefix p t
      | M_none ->
        check_nonempty ~path:prefix t; Trie.empty
      | M_in (p, pat) ->
        Trie.update_subtree p (go ~prefix:(prefix <>< p) pat) t
      | M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        check_nonempty ~path:(prefix <>< p1) t;
        Trie.union_subtree ~prefix merger remaining (p2, t)
      | M_seq pats ->
        let f t pat = go ~prefix pat t in
        List.fold_left ~f ~init:t pats
      | M_union pats ->
        let f ts pat =
          let ti = go ~prefix pat t in
          Trie.union ~prefix merger ts ti
        in
        List.fold_left ~f ~init:Trie.empty pats
      | M_hook h -> hook h ~prefix t
    in go

  let run ?source ?(prefix=Emp) pat t = run_ ~source ~prefix pat t
end
