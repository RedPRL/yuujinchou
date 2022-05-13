module type Param = Modifier.Param

module type S =
sig
  include Param

  exception Locked

  module Mod : Modifier.S with type data = data and type hook = hook
  type Mod.source += Visible | Export

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

module Make (P : Param) : S with type data = P.data and type hook = P.hook
