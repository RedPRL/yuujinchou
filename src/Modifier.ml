open StdLabels
open Algaeff.StdlibShim

open Bwd
open BwdNotation

open Language

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
    | BindingNotFound : {source : source option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {source : source option; path : Trie.bwd_path; former : data; latter : data} -> data Effect.t
    | Hook : {source : source option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> data Trie.t Effect.t

  val exec : ?source:source -> ?prefix:Trie.bwd_path -> hook modifier -> data Trie.t -> data Trie.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook =
struct
  include P

  type source = ..

  type _ Effect.t +=
    | BindingNotFound : {source : source option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {source : source option; path : Trie.bwd_path; former : data; latter : data} -> data Effect.t
    | Hook : {source : source option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> data Trie.t Effect.t

  let check_nonempty ~source ~prefix t =
    if Trie.is_empty t then
      Effect.perform @@ BindingNotFound {source; prefix}
  let merger ~source ~path former latter = Effect.perform @@ Shadowing {source; path; former; latter}
  let do_hook ~source ~hook ~prefix t = Effect.perform @@ Hook {source; prefix; hook; input=t}

  let exec ?source ?(prefix=Emp) =
    let rec go ~prefix m t =
      match m with
      | M_only p ->
        let t = Trie.find_subtree p t in
        check_nonempty ~source ~prefix:(prefix <>< p) t;
        Trie.prefix p t
      | M_none ->
        check_nonempty ~source ~prefix t; Trie.empty
      | M_in (p, m) ->
        Trie.update_subtree p (go ~prefix:(prefix <>< p) m) t
      | M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        check_nonempty ~source ~prefix:(prefix <>< p1) t;
        Trie.union_subtree ~prefix (merger ~source) remaining (p2, t)
      | M_seq ms ->
        let f t m = go ~prefix m t in
        List.fold_left ~f ~init:t ms
      | M_union ms ->
        let f ts m =
          let ti = go ~prefix m t in
          Trie.union ~prefix (merger ~source) ts ti
        in
        List.fold_left ~f ~init:Trie.empty ms
      | M_hook hook -> do_hook ~source ~hook ~prefix t
    in go ~prefix
end
