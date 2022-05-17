module type Param = Modifier.Param

module type S =
sig
  include Param

  exception Locked

  module Mod : Modifier.S with type data = data and type tag = tag and type hook = hook and type context = context

  val resolve : Trie.path -> (data * tag) option
  val modify_visible : ?context:context -> hook Language.modifier -> unit
  val modify_export : ?context:context -> hook Language.modifier -> unit
  val export_visible : ?context:context -> hook Language.modifier -> unit
  val include_singleton : ?context_visible:context -> ?context_export:context -> Trie.path * (data * tag) -> unit
  val include_subtree : ?context_visible:context -> ?context_export:context -> Trie.path * (data, tag) Trie.t -> unit
  val import_subtree : ?context:context -> Trie.path * (data, tag) Trie.t -> unit
  val get_export : unit -> (data, tag) Trie.t
  val section : ?context_visible:context -> ?context_export:context -> Trie.path -> (unit -> 'a) -> 'a
  val run : ?prefix:Trie.bwd_path -> (unit -> 'a) -> 'a
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
