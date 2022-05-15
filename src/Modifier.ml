open StdLabels

open Bwd
open BwdNotation

open Language

module type Param =
sig
  type data
  type hook
  type caller
end

module type S =
sig
  include Param

  type _ Effect.t +=
    | BindingNotFound : {caller : caller option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {caller : caller option; path : Trie.bwd_path; former : data; latter : data} -> data Effect.t
    | Hook : {caller : caller option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> data Trie.t Effect.t

  val exec : ?caller:caller -> ?prefix:Trie.bwd_path -> hook modifier -> data Trie.t -> data Trie.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook and type caller = P.caller =
struct
  include P

  type _ Effect.t +=
    | BindingNotFound : {caller : caller option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {caller : caller option; path : Trie.bwd_path; former : data; latter : data} -> data Effect.t
    | Hook : {caller : caller option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> data Trie.t Effect.t

  let check_nonempty ~caller ~prefix t =
    if Trie.is_empty t then
      Effect.perform @@ BindingNotFound {caller; prefix}
  let merger ~caller path former latter = Effect.perform @@ Shadowing {caller; path; former; latter}
  let do_hook ~caller ~hook ~prefix t = Effect.perform @@ Hook {caller; prefix; hook; input=t}

  let exec ?caller ?(prefix=Emp) =
    let rec go ~prefix m t =
      match m with
      | M_only p ->
        let t = Trie.find_subtree p t in
        check_nonempty ~caller ~prefix:(prefix <>< p) t;
        Trie.prefix p t
      | M_none ->
        check_nonempty ~caller ~prefix t; Trie.empty
      | M_in (p, m) ->
        Trie.update_subtree p (go ~prefix:(prefix <>< p) m) t
      | M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        check_nonempty ~caller ~prefix:(prefix <>< p1) t;
        Trie.union_subtree ~prefix (merger ~caller) remaining (p2, t)
      | M_seq ms ->
        let f t m = go ~prefix m t in
        List.fold_left ~f ~init:t ms
      | M_union ms ->
        let f ts m =
          let ti = go ~prefix m t in
          Trie.union ~prefix (merger ~caller) ts ti
        in
        List.fold_left ~f ~init:Trie.empty ms
      | M_hook hook -> do_hook ~caller ~hook ~prefix t
    in go ~prefix
end
