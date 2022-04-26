module type Param = Action.Param

module type S =
sig
  include Param

  exception Locked

  module Act : Action.S with type data = data and type hook = hook
  type Act.source += Visible | Export

  val resolve : Trie.path -> data option
  val run_on_visible : hook Modifier.t -> unit
  val run_on_export : hook Modifier.t -> unit
  val export_visible : hook Modifier.t -> unit
  val include_singleton : Trie.path * data -> unit
  val include_subtree : Trie.path * data Trie.t -> unit
  val import_subtree : Trie.path * data Trie.t -> unit
  val section : Trie.path -> (unit -> 'a) -> 'a
  val run : ?prefix:Trie.bwd_path -> (unit -> 'a) -> 'a
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook
