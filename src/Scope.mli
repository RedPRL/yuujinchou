module type Param = Action.Param

module type S =
sig
  include Param

  exception Locked

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

module Make (P : Param) : S with type data = P.data and type hook = P.hook
