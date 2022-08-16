open Bwd
open Bwd.Infix

module type Param = ModifierSigs.Param
module type Handler = ModifierSigs.Handler
module type S = ModifierSigs.S with module Language := Language

module Make (Param : Param) : S with module Param := Param =
struct
  module type Handler = Handler with module Param := Param

  module Language = Language
  open Param

  module Perform =
  struct
    type _ Effect.t +=
      | NotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
      | Shadow : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t
      | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t

    let not_found context prefix = Effect.perform @@ NotFound {context; prefix}
    let shadow context path former latter = Effect.perform @@ Shadow {context; path; former; latter}
    let hook context prefix hook input = Effect.perform @@ Hook {context; prefix; hook; input}
  end

  open Perform

  let modify ?context ?(prefix=Emp) =
    let module L = Language in
    let rec go prefix m t =
      match m with
      | L.M_assert_nonempty ->
        if Trie.is_empty t then not_found context prefix; t
      | L.M_in (p, m) ->
        Trie.update_subtree p (go (prefix <>< p) m) t
      | L.M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        Trie.update_subtree p2 (fun _ -> t) remaining
      | L.M_seq ms ->
        let f t m = go prefix m t in
        List.fold_left f t ms
      | L.M_union ms ->
        let f ts m =
          let ti = go prefix m t in
          Trie.union ~prefix (shadow context) ts ti
        in
        List.fold_left f Trie.empty ms
      | L.M_hook id -> hook context prefix id t
    in go prefix

  module Run (H : Handler) =
  struct
    open Effect.Deep

    let handler (type a) : a Effect.t -> _ =
      function
      | NotFound {context; prefix} -> Option.some @@ fun (k : (a, _) continuation) ->
        Algaeff.Fun.Deep.finally k @@ fun () -> H.not_found context prefix
      | Shadow {context; path; former; latter} -> Option.some @@ fun (k : (a, _) continuation) ->
        Algaeff.Fun.Deep.finally k @@ fun () -> H.shadow context path former latter
      | Hook {context; prefix; hook; input} -> Option.some @@ fun (k : (a, _) continuation) ->
        Algaeff.Fun.Deep.finally k @@ fun () -> H.hook context prefix hook input
      | _ -> None

    let run f = try_with f () {effc = handler}
  end

  module TryWith (H : Handler) =
  struct
    module R = Run (H)
    let try_with = R.run
  end
end
