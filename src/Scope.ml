open Bwd
open BwdNotation
open ScopeSigs

module type Param = ScopeSigs.Param
module type S = S with module Language := Language

module Make (Param : Param) : S with module Param := Param =
struct
  open Param
  module type Handler = ScopeSigs.Handler with module Param := Param
  type handler = (module Handler)

  module Internal =
  struct
    module Mod = Modifier.Make(Param)

    module M = Algaeff.Mutex.Make()

    type scope = {visible : (data, tag) Trie.t; export : (data, tag) Trie.t}
    module S = Algaeff.State.Make(struct type state = scope end)

    type env = {export_prefix : Trie.bwd_path}
    module R = Algaeff.Reader.Make(struct type nonrec env = env end)

    let run ~export_prefix ~init_visible f =
      let env = {export_prefix} in
      let init = {visible = init_visible; export = Trie.empty} in
      M.run @@ fun () -> R.run ~env @@ fun () -> S.run ~init f

    let export_prefix () = (R.read()).export_prefix
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
    {s with export = Mod.modify ?context ~prefix:(export_prefix()) m s.export}

  let modify = Mod.modify

  let export_visible ?context m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with
     export =
       Trie.union ~prefix:(export_prefix()) (Mod.Perform.shadow context) s.export @@
       Mod.modify ?context ~prefix:Emp m s.visible }

  let include_singleton ?context_visible ?context_export (path, x) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { visible = Trie.union_singleton ~prefix:Emp (Mod.Perform.shadow context_visible) s.visible (path, x);
      export = Trie.union_singleton ~prefix:(export_prefix()) (Mod.Perform.shadow context_export) s.export (path, x) }

  let unsafe_include_subtree ~context_visible ~context_export (path, ns) =
    S.modify @@ fun s ->
    { visible = Trie.union_subtree ~prefix:Emp (Mod.Perform.shadow context_visible) s.visible (path, ns);
      export = Trie.union_subtree ~prefix:(export_prefix()) (Mod.Perform.shadow context_export) s.export (path, ns) }

  let include_subtree ?context_visible ?context_export (path, ns) =
    M.exclusively @@ fun () -> unsafe_include_subtree ~context_visible ~context_export (path, ns)

  let import_subtree ?context (path, ns) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { s with visible = Trie.union_subtree ~prefix:Emp (Mod.Perform.shadow context) s.visible (path, ns) }

  let get_export () =
    M.exclusively @@ fun () -> (S.get()).export

  let section ?context_visible ?context_export p f =
    M.exclusively @@ fun () ->
    let ans, export =
      Internal.run ~export_prefix:(export_prefix() <>< p) ~init_visible:(S.get()).visible @@ fun () ->
      let ans = f () in ans, get_export ()
    in
    unsafe_include_subtree ~context_visible ~context_export (p, export);
    ans

  let run ?(export_prefix=Emp) ?(init_visible=Trie.empty) h f =
    Mod.run h @@ fun () -> Internal.run ~export_prefix ~init_visible f

  let try_with = Mod.try_with

  module Perform = Mod.Perform
  let perform = Mod.perform
end
