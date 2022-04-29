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

  val exec : ?source:source -> ?prefix:Trie.bwd_path -> hook Modifier.t -> data Trie.t -> data Trie.t
end

module Make (P : Param) =
struct
  include P

  type source = ..

  type _ Effect.t +=
    | BindingNotFound : source option * Trie.bwd_path -> unit Effect.t
    | Shadowing : source option * Trie.bwd_path * data * data -> data Effect.t
    | Hook : source option * Trie.bwd_path * hook * data Trie.t -> data Trie.t Effect.t

  let exec ?source ?(prefix=Emp) =
    let check_nonempty ~path t =
      if Trie.is_empty t then
        Effect.perform @@ BindingNotFound (source, path)
    in
    let merger ~path x y = Effect.perform @@ Shadowing (source, path, x, y) in
    let hook h ~prefix t = Effect.perform @@ Hook (source, prefix, h, t) in
    let rec go ~prefix m t =
      match m with
      | M_only p ->
        let t = Trie.find_subtree p t in
        check_nonempty ~path:(prefix <>< p) t;
        Trie.prefix p t
      | M_none ->
        check_nonempty ~path:prefix t; Trie.empty
      | M_in (p, m) ->
        Trie.update_subtree p (go ~prefix:(prefix <>< p) m) t
      | M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        check_nonempty ~path:(prefix <>< p1) t;
        Trie.union_subtree ~prefix merger remaining (p2, t)
      | M_seq ms ->
        let f t m = go ~prefix m t in
        List.fold_left ~f ~init:t ms
      | M_union ms ->
        let f ts m =
          let ti = go ~prefix m t in
          Trie.union ~prefix merger ts ti
        in
        List.fold_left ~f ~init:Trie.empty ms
      | M_hook h -> hook h ~prefix t
    in go ~prefix

end
