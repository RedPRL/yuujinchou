open Bwd
open Bwd.Infix

module type Param = ModifierSigs.Param
module type Perform = ModifierSigs.Perform
module type S = ModifierSigs.S with module Language := Language

module Make (Param : Param) : S with module Param := Param =
struct
  module Language = Language
  open Param

  type not_found_handler = context option -> Trie.bwd_path -> unit
  type shadow_handler = context option -> Trie.bwd_path -> data * tag -> data * tag -> data * tag
  type hook_handler = context option -> Trie.bwd_path -> hook -> (data, tag) Trie.t -> (data, tag) Trie.t

  type _ Effect.t +=
    | NotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadow : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t
    | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t

  module type Perform = Perform with module Param := Param

  module Perform : Perform =
  struct
    let not_found context prefix = Effect.perform @@ NotFound {context; prefix}
    let shadow context path former latter = Effect.perform @@ Shadow {context; path; former; latter}
    let hook context prefix hook input = Effect.perform @@ Hook {context; prefix; hook; input}
  end

  module Silence : Perform =
  struct
    let not_found _ _ = ()
    let shadow _ _ _ d = d
    let hook _ _ _ t = t
  end

  open Perform

  let union ?context ?prefix t1 t2 =
    Trie.union ?prefix (shadow context) t1 t2

  let union_subtree ?context ?prefix t1 (p, t2) =
    Trie.union_subtree ?prefix (shadow context) t1 (p, t2)

  let union_singleton ?context ?prefix t b =
    Trie.union_singleton ?prefix (shadow context) t b

  let union_root ?context ?prefix t v =
    Trie.union_root ?prefix (shadow context) t v

  let modify ?context ?(prefix=Emp) =
    let module L = Language in
    let rec go prefix m t =
      match m with
      | L.M_assert_nonempty ->
        if Trie.is_empty t then not_found context prefix; t
      | L.M_in (p, m) ->
        Trie.update_subtree p (go (prefix <@ p) m) t
      | L.M_renaming (p1, p2) ->
        let t, remaining = Trie.detach_subtree p1 t in
        Trie.update_subtree p2 (fun _ -> t) remaining
      | L.M_seq ms ->
        let f t m = go prefix m t in
        List.fold_left f t ms
      | L.M_union ms ->
        let f ts m =
          let ti = go prefix m t in
          union ?context ~prefix ts ti
        in
        List.fold_left f Trie.empty ms
      | L.M_hook id -> hook context prefix id t
    in go prefix

  let handler ~(not_found:not_found_handler) ~(shadow:shadow_handler) ~(hook:hook_handler) : _ Effect.Deep.effect_handler =
    {effc =
       fun (type a) (eff : a Effect.t) ->
         match eff with
         | NotFound {context; prefix} -> Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
           Algaeff.Fun.Deep.finally k @@ fun () -> not_found context prefix
         | Shadow {context; path; former; latter} -> Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
           Algaeff.Fun.Deep.finally k @@ fun () -> shadow context path former latter
         | Hook {context; prefix; hook=hookName; input} -> Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
           Algaeff.Fun.Deep.finally k @@ fun () -> hook context prefix hookName input
         | _ -> None
    }

  let run ?(not_found=Silence.not_found) ?(shadow=Silence.shadow) ?(hook=Silence.hook) f =
    Effect.Deep.try_with f () @@ handler ~not_found ~shadow ~hook

  let try_with ?(not_found=Perform.not_found) ?(shadow=Perform.shadow) ?(hook=Perform.hook) f = run ~not_found ~shadow ~hook f

  let register_printer f =
    Printexc.register_printer @@ function
    | Effect.Unhandled (NotFound {context; prefix}) -> f (`NotFound (context, prefix))
    | Effect.Unhandled (Shadow {context; path; former; latter}) -> f (`Shadow (context, path, former, latter))
    | Effect.Unhandled (Hook {context; prefix; hook; input}) -> f (`Hook (context, prefix, hook, input))
    | _ -> None

  let () = register_printer @@ fun _ -> Some "Unhandled yuujinchou effect; use Yuujinchou.Modifier.run"
end
