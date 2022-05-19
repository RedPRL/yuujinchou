open Bwd
open BwdNotation

type ('data, 'tag, 'hook, 'context) handler = ('data, 'tag, 'hook, 'context) Modifier.handler

module type Param = Modifier.Param

module type S =
sig
  include Param

  exception Locked

  val resolve : Trie.path -> (data * tag) option
  val modify_visible : ?context:context -> hook Language.t -> unit
  val modify_export : ?context:context -> hook Language.t -> unit
  val export_visible : ?context:context -> hook Language.t -> unit
  val include_singleton : ?context_visible:context -> ?context_export:context -> Trie.path * (data * tag) -> unit
  val include_subtree : ?context_visible:context -> ?context_export:context -> Trie.path * (data, tag) Trie.t -> unit
  val import_subtree : ?context:context -> Trie.path * (data, tag) Trie.t -> unit
  val get_export : unit -> (data, tag) Trie.t
  val section : ?context_visible:context -> ?context_export:context -> Trie.path -> (unit -> 'a) -> 'a
  val run : ?prefix:Trie.bwd_path -> (unit -> 'a) -> (data, tag, hook, context) handler -> 'a

  val run_modifier : (unit -> 'a) -> (data, tag, hook, context) handler -> 'a
  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  val reperform : (data, tag, hook, context) handler
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context =
struct
  include P

  module Internal =
  struct
    module Mod = Modifier.Make(P)

    module M = Algaeff.Mutex.Make()

    type scope = {visible : (data, tag) Trie.t; export : (data, tag) Trie.t}
    module S = Algaeff.State.Make(struct type state = scope end)

    type env = {prefix : Trie.bwd_path}
    module R = Algaeff.Reader.Make(struct type nonrec env = env end)

    let run ~prefix ~init_visible f =
      let env = {prefix} in
      let init = {visible = init_visible; export = Trie.empty} in
      M.run @@ fun () -> R.run ~env @@ fun () -> S.run ~init f

    let prefix () = (R.read()).prefix
  end

  open Internal

  exception Locked = M.Locked

  let resolve p =
    M.exclusively @@ fun () ->
    Trie.find_singleton p (S.get ()).visible

  let modify_visible ?context m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with visible = Mod.modify ?context ~prefix:Emp m s.visible}

  let modify_export ?context m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with export = Mod.modify ?context ~prefix:(prefix()) m s.export}

  let export_visible ?context m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with
     export =
       Trie.union ~prefix:(prefix()) (Mod.reperform.shadow ?context) s.export @@
       Mod.modify ?context ~prefix:Emp m s.visible }

  let include_singleton ?context_visible ?context_export (path, x) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { visible = Trie.union_singleton ~prefix:Emp (Mod.reperform.shadow ?context:context_visible) s.visible (path, x);
      export = Trie.union_singleton ~prefix:(prefix()) (Mod.reperform.shadow ?context:context_export) s.export (path, x) }

  let unsafe_include_subtree ~context_visible ~context_export (path, ns) =
    S.modify @@ fun s ->
    { visible = Trie.union_subtree ~prefix:Emp (Mod.reperform.shadow ?context:context_visible) s.visible (path, ns);
      export = Trie.union_subtree ~prefix:(prefix()) (Mod.reperform.shadow ?context:context_export) s.export (path, ns) }

  let include_subtree ?context_visible ?context_export (path, ns) =
    M.exclusively @@ fun () -> unsafe_include_subtree ~context_visible ~context_export (path, ns)

  let import_subtree ?context (path, ns) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { s with visible = Trie.union_subtree ~prefix:Emp (Mod.reperform.shadow ?context) s.visible (path, ns) }

  let get_export () =
    M.exclusively @@ fun () -> (S.get()).export

  let section ?context_visible ?context_export p f =
    M.exclusively @@ fun () ->
    let ans, export =
      Internal.run ~prefix:(prefix() <>< p) ~init_visible:(S.get()).visible @@ fun () ->
      let ans = f () in ans, get_export ()
    in
    unsafe_include_subtree ~context_visible ~context_export (p, export);
    ans

  let run ?(prefix=Emp) f h =
    Mod.run (fun () -> Internal.run ~prefix ~init_visible:Trie.empty f) h

  let run_modifier = Mod.run
  let modify = Mod.modify
  let reperform = Mod.reperform
end
