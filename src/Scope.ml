open Bwd
open BwdNotation

type Modifier.source += Visible | Export

module type Param =
sig
  type data
  type hook
end

module type S =
sig
  include Param

  exception Locked

  module Mod : Modifier.S with type data = data and type hook = hook

  val resolve : Trie.path -> data option
  val modify_visible : hook Language.modifier -> unit
  val modify_export : hook Language.modifier -> unit
  val export_visible : hook Language.modifier -> unit
  val include_singleton : Trie.path * data -> unit
  val include_subtree : Trie.path * data Trie.t -> unit
  val import_subtree : Trie.path * data Trie.t -> unit
  val get_export : unit -> data Trie.t
  val section : Trie.path -> (unit -> 'a) -> 'a
  val run : ?prefix:Trie.bwd_path -> (unit -> 'a) -> 'a
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook =
struct
  include P

  module Internal =
  struct
    module Mod = Modifier.Make(P)

    module M = Algaeff.Mutex.Make()

    type scope = {visible : data Trie.t; export : data Trie.t}
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

  module Mod = Internal.Mod

  let resolve p =
    M.exclusively @@ fun () ->
    Trie.find_singleton p (S.get ()).visible

  let modify_visible m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with visible = Mod.exec ~source:Visible ~prefix:Emp m s.visible}

  let modify_export m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with export = Mod.exec ~source:Export ~prefix:(prefix()) m s.export}

  let merger ~source ~path former latter = Effect.perform @@ Mod.Shadowing {source = Some source; path; former; latter}

  let export_visible m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with
     export =
       Trie.union ~prefix:(prefix()) (merger ~source:Export) s.export @@
       Mod.exec ~source:Visible ~prefix:Emp m s.visible }

  let include_singleton (path, x) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { visible = Trie.union_singleton ~prefix:Emp (merger ~source:Visible) s.visible (path, x);
      export = Trie.union_singleton ~prefix:(prefix()) (merger ~source:Export) s.export (path, x) }

  let unsafe_include_subtree (path, ns) =
    S.modify @@ fun s ->
    { visible = Trie.union_subtree ~prefix:Emp (merger ~source:Visible) s.visible (path, ns);
      export = Trie.union_subtree ~prefix:(prefix()) (merger ~source:Export) s.export (path, ns) }

  let include_subtree (path, ns) =
    M.exclusively @@ fun () -> unsafe_include_subtree (path, ns)

  let import_subtree (path, ns) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { s with visible = Trie.union_subtree ~prefix:Emp (merger ~source:Visible) s.visible (path, ns) }

  let get_export () =
    M.exclusively @@ fun () -> (S.get()).export

  let section p f =
    M.exclusively @@ fun () ->
    let ans, export =
      Internal.run ~prefix:(prefix() <>< p) ~init_visible:(S.get()).visible @@ fun () ->
      let ans = f () in ans, get_export ()
    in
    unsafe_include_subtree (p, export);
    ans

  let run ?(prefix=Emp) f = Internal.run ~prefix ~init_visible:Trie.empty f
end
