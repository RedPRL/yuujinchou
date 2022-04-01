open Eff.StdlibShim

module type Param = Action.Param

module type S =
sig
  include Param

  exception RecursiveLocking

  module Act : Action.S with type data = data and type hook = hook

  val resolve : Trie.path -> data option
  val run_on_visible : ?prefix:Trie.bwd_path -> hook Pattern.t -> unit
  val run_on_export : ?prefix:Trie.bwd_path -> hook Pattern.t -> unit
  val export_visible : ?prefix:Trie.bwd_path -> hook Pattern.t -> unit
  val include_singleton : ?prefix:Trie.bwd_path -> Trie.path * data -> unit
  val include_subtree : ?prefix:Trie.bwd_path -> Trie.path * data Trie.t -> unit
  val import_subtree : ?prefix:Trie.bwd_path -> Trie.path * data Trie.t -> unit
  val run : (unit -> 'a) -> 'a
  val section : ?prefix:Trie.bwd_path -> Trie.path -> (unit -> 'a) -> 'a
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook =
struct
  include P

  module Internal =
  struct
    module A = Action.Make(P)

    module M = Eff.Mutex.Make()

    type scope = {visible : data Trie.t; export : data Trie.t}
    module S = Eff.State.Make(struct type state = scope end)

    let run_scope ~init_visible f =
      let init = {visible = init_visible; export = Trie.empty} in
      M.run @@ fun () -> S.run ~init f
  end

  open Internal

  exception RecursiveLocking = M.RecursiveLocking

  module Act = Internal.A

  let resolve p =
    M.exclusively @@ fun () ->
    Trie.find_singleton p (S.get ()).visible

  let run_on_visible ?prefix pat =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with visible = A.run ?prefix pat s.visible}

  let run_on_export ?prefix pat =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with export = A.run ?prefix pat s.export}

  let merger ~path x y = Effect.perform @@ Act.Shadowing (path, x, y)

  let export_visible ?prefix pat =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with
     export =
       Trie.union ?prefix merger s.export @@
       A.run ?prefix pat s.visible }

  let include_singleton ?prefix (path, x) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { visible = Trie.union_singleton ?prefix merger s.visible (path, x);
      export = Trie.union_singleton ?prefix merger s.export (path, x) }

  let unsafe_include_subtree ?prefix (path, ns) =
    S.modify @@ fun s ->
    { visible = Trie.union_subtree ?prefix merger s.visible (path, ns);
      export = Trie.union_subtree ?prefix merger s.export (path, ns) }

  let include_subtree ?prefix (path, ns) =
    M.exclusively @@ fun () -> unsafe_include_subtree ?prefix (path, ns)

  let import_subtree ?prefix (path, ns) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { s with visible = Trie.union_subtree ?prefix merger s.visible (path, ns) }

  let run f = run_scope ~init_visible:Trie.empty f

  let section ?prefix p f =
    M.exclusively @@ fun () ->
    let ans, export =
      run_scope ~init_visible:(S.get()).visible @@ fun () ->
      let ans = f () in ans, (S.get()).export
    in
    unsafe_include_subtree ?prefix (p, export);
    ans
end
