open StdLabels

open Bwd
open BwdNotation

type ('data, 'tag, 'hook, 'context) handler = {
  not_found : ?context:'context -> Trie.bwd_path -> unit;
  shadow : ?context:'context -> Trie.bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag;
  hook : ?context:'context -> Trie.bwd_path -> 'hook -> ('data, 'tag) Trie.t -> ('data, 'tag) Trie.t;
}

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

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  val run : (unit -> 'a) -> (data, tag, hook, context) handler -> 'a

  val reperform : (data, tag, hook, context) handler
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context =
struct
  include P

  module Internal =
  struct
    type _ Effect.t +=
      | NotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
      | Shadow : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t
      | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t
    let not_found ~context prefix = Effect.perform @@ NotFound {context; prefix}
    let shadow ~context path former latter = Effect.perform @@ Shadow {context; path; former; latter}
    let hook ~context prefix hook input = Effect.perform @@ Hook {context; prefix; hook; input}

    let check_nonempty ~context prefix t =
      if Trie.is_empty t then not_found ~context prefix
  end

  open Internal

  let modify ?context ?(prefix=Emp) =
    let module L = Language in
    let rec go prefix m t =
      match m with
      | L.M_none ->
        check_nonempty ~context prefix t; Trie.empty
      | L.M_in (p, m) ->
        Trie.update_subtree p (go (prefix <>< p) m) t
      | L.M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        check_nonempty ~context (prefix <>< p1) t;
        Trie.update_subtree p2 (fun _ -> t) remaining
      | L.M_seq ms ->
        let f t m = go prefix m t in
        List.fold_left ~f ~init:t ms
      | L.M_union ms ->
        let f ts m =
          let ti = go prefix m t in
          Trie.union ~prefix (shadow ~context) ts ti
        in
        List.fold_left ~f ~init:Trie.empty ms
      | L.M_hook id -> hook ~context prefix id t
    in go prefix

  let run f h =
    let open Effect.Deep in
    try_with f ()
      { effc = fun (type a) (eff : a Effect.t) ->
            match eff with
            | NotFound {context; prefix} -> Option.some @@ fun (k : (a, _) continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> h.not_found ?context prefix
            | Shadow {context; path; former; latter} -> Option.some @@ fun (k : (a, _) continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> h.shadow ?context path former latter
            | Hook {context; prefix; hook; input}-> Option.some @@ fun (k : (a, _) continuation) ->
              Algaeff.Fun.Deep.finally k @@ fun () -> h.hook ?context prefix hook input
            | _ -> None }

  let reperform =
    { not_found = (fun ?context -> Internal.not_found ~context);
      shadow = (fun ?context -> Internal.shadow ~context);
      hook = (fun ?context -> Internal.hook ~context) }
end
