module type Param = Modifier.Param

module type S =
sig
  include Param

  exception Locked

  module Mod : Modifier.S with type data = data and type hook = hook and type caller = caller

  val resolve : Trie.path -> data option
  val modify_visible : ?caller:caller -> hook Language.modifier -> unit
  val modify_export : ?caller:caller -> hook Language.modifier -> unit
  val export_visible : ?caller:caller -> hook Language.modifier -> unit
  val include_singleton : ?caller_visible:caller -> ?caller_export:caller -> Trie.path * data -> unit
  val include_subtree : ?caller_visible:caller -> ?caller_export:caller -> Trie.path * data Trie.t -> unit
  val import_subtree : ?caller:caller -> Trie.path * data Trie.t -> unit
  val get_export : unit -> data Trie.t
  val section : ?caller_visible:caller -> ?caller_export:caller -> Trie.path -> (unit -> 'a) -> 'a
  val run : ?prefix:Trie.bwd_path -> (unit -> 'a) -> 'a
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook and type caller = P.caller
