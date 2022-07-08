open Bwd
open BwdNotation

module type Param =
sig
  type data
  type tag
  type hook
  type context
end

module type Handler =
sig
  module P : Param
  val not_found : P.context option -> Trie.bwd_path -> unit
  val shadow : P.context option -> Trie.bwd_path -> P.data * P.tag -> P.data * P.tag -> P.data * P.tag
  val hook : P.context option -> Trie.bwd_path -> P.hook -> (P.data, P.tag) Trie.t -> (P.data, P.tag) Trie.t
end


module type S =
sig
  module P : Param
  open P

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t

  module Handle (H : Handler with module P := P) :
  sig
    val run : (unit -> 'a) -> 'a
    val try_with : (unit -> 'a) -> 'a
  end

  module Perform : Handler with module P := P
end

module Make (P : Param) : S with module P = P =
struct
  module P = P
  include P

  module Internal =
  struct
    type _ Effect.t +=
      | NotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
      | Shadow : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t
      | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t
    let not_found context prefix = Effect.perform @@ NotFound {context; prefix}
    let shadow context path former latter = Effect.perform @@ Shadow {context; path; former; latter}
    let hook context prefix hook input = Effect.perform @@ Hook {context; prefix; hook; input}
  end

  open Internal

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

  module Handle (H : Handler with module P := P) =
  struct

    let run f =
      let open Effect.Deep in
      try_with f ()
        { effc = fun (type a) (eff : a Effect.t) ->
              match eff with
              | NotFound {context; prefix} -> Option.some @@ fun (k : (a, _) continuation) ->
                Algaeff.Fun.Deep.finally k @@ fun () -> H.not_found context prefix
              | Shadow {context; path; former; latter} -> Option.some @@ fun (k : (a, _) continuation) ->
                Algaeff.Fun.Deep.finally k @@ fun () -> H.shadow context path former latter
              | Hook {context; prefix; hook; input}-> Option.some @@ fun (k : (a, _) continuation) ->
                Algaeff.Fun.Deep.finally k @@ fun () -> H.hook context prefix hook input
              | _ -> None }

    let try_with = run
  end

  module Perform =
  struct
    module P = P
    include Internal
  end

end
