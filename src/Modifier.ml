open StdLabels

open Bwd
open BwdNotation

open Language

module type Param =
sig
  type data
  type tag
  type hook
  type context
end

module type S =
sig
  include Param

  type _ Effect.t +=
    | BindingNotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t
    | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t

  val exec : ?context:context -> ?prefix:Trie.bwd_path -> hook modifier -> (data, tag) Trie.t -> (data, tag) Trie.t
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context =
struct
  include P

  type _ Effect.t +=
    | BindingNotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t
    | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t

  let check_nonempty ~context ~prefix t =
    if Trie.is_empty t then
      Effect.perform @@ BindingNotFound {context; prefix}
  let merger ~context path former latter = Effect.perform @@ Shadowing {context; path; former; latter}
  let do_hook ~context ~hook ~prefix t = Effect.perform @@ Hook {context; prefix; hook; input=t}

  let exec ?context ?(prefix=Emp) =
    let rec go ~prefix m t =
      match m with
      | M_only p ->
        let t = Trie.find_subtree p t in
        check_nonempty ~context ~prefix:(prefix <>< p) t;
        Trie.prefix p t
      | M_none ->
        check_nonempty ~context ~prefix t; Trie.empty
      | M_in (p, m) ->
        Trie.update_subtree p (go ~prefix:(prefix <>< p) m) t
      | M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        check_nonempty ~context ~prefix:(prefix <>< p1) t;
        Trie.union_subtree ~prefix (merger ~context) remaining (p2, t)
      | M_seq ms ->
        let f t m = go ~prefix m t in
        List.fold_left ~f ~init:t ms
      | M_union ms ->
        let f ts m =
          let ti = go ~prefix m t in
          Trie.union ~prefix (merger ~context) ts ti
        in
        List.fold_left ~f ~init:Trie.empty ms
      | M_hook hook -> do_hook ~context ~hook ~prefix t
    in go ~prefix
end
