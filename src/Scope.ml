open Bwd
open Bwd.Infix
open ScopeSigs

module type Param = ScopeSigs.Param
module type Perform = ScopeSigs.Perform
module type S = S with module Language := Language

module Make (Param : Param) : S with module Param := Param =
struct
  open Param

  type not_found_handler = context option -> Trie.bwd_path -> unit
  type shadow_handler = context option -> Trie.bwd_path -> data * tag -> data * tag -> data * tag
  type hook_handler = context option -> Trie.bwd_path -> hook -> (data, tag) Trie.t -> (data, tag) Trie.t

  module Internal =
  struct
    module Mod = Modifier.Make(Param)

    module M = Algaeff.Mutex.Make()

    type scope = {visible : (data, tag) Trie.t; export : (data, tag) Trie.t}
    module S = Algaeff.State.Make(struct type t = scope end)

    type env = {export_prefix : Trie.bwd_path}
    module R = Algaeff.Reader.Make(struct type t = env end)

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

  let modify_visible ?context_visible m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with visible = Mod.modify ?context:context_visible ~prefix:Emp m s.visible}

  let modify_export ?context_export m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with export = Mod.modify ?context:context_export ~prefix:(export_prefix()) m s.export}

  let export_visible ?context_modifier ?context_export m =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    {s with
     export =
       Mod.union ?context:context_export ~prefix:(export_prefix()) s.export @@
       Mod.modify ?context:context_modifier m s.visible }

  let include_singleton ?context_visible ?context_export (path, x) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { visible = Mod.union_singleton ?context:context_visible s.visible (path, x);
      export = Mod.union_singleton ?context:context_export ~prefix:(export_prefix()) s.export (path, x) }

  let import_singleton ?context_visible (path, x) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    { s with visible = Mod.union_singleton ?context:context_visible s.visible (path, x) }

  let unsafe_include_subtree ~context_modifier ~context_visible ~context_export ~modifier (path, ns) =
    S.modify @@ fun s ->
    let ns = Mod.modify ?context:context_modifier ~prefix:Emp modifier ns in
    { visible = Mod.union_subtree ?context:context_visible s.visible (path, ns);
      export = Mod.union_subtree ?context:context_export ~prefix:(export_prefix()) s.export (path, ns) }

  let include_subtree ?context_modifier ?context_visible ?context_export ?(modifier=Language.id) (path, ns) =
    M.exclusively @@ fun () -> unsafe_include_subtree ~context_modifier ~context_visible ~context_export ~modifier (path, ns)

  let import_subtree ?context_modifier ?context_visible ?(modifier=Language.id) (path, ns) =
    M.exclusively @@ fun () -> S.modify @@ fun s ->
    let ns = Mod.modify ?context:context_modifier ~prefix:Emp modifier ns in
    { s with visible = Mod.union_subtree ?context:context_visible s.visible (path, ns) }

  let get_visible () =
    M.exclusively @@ fun () -> (S.get()).visible

  let get_export () =
    M.exclusively @@ fun () -> (S.get()).export

  let section ?context_modifier ?context_visible ?context_export ?(modifier=Language.id) p f =
    M.exclusively @@ fun () ->
    let ans, export =
      Internal.run ~export_prefix:(export_prefix() <@ p) ~init_visible:(S.get()).visible @@ fun () ->
      let ans = f () in ans, get_export ()
    in
    unsafe_include_subtree ~context_modifier ~context_visible ~context_export ~modifier (p, export);
    ans

  let run ?not_found ?shadow ?hook ?(export_prefix=Emp) ?(init_visible=Trie.empty) f =
    Mod.run ?not_found ?shadow ?hook @@ fun () -> Internal.run ~export_prefix ~init_visible f

  let try_with = Mod.try_with

  let register_printer = Mod.register_printer

  (* This overrides the default printer registered by Mod. *)
  let () = register_printer @@ fun _ -> Some "Unhandled yuujinchou effect; use Yuujinchou.Scope.run"

  module type Perform = Mod.Perform
  module Perform = Mod.Perform
  module Silence = Mod.Silence
end
