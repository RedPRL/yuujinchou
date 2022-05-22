type ('data, 'tag, 'hook, 'context) handler = ('data, 'tag, 'hook, 'context) Modifier.handler

module type Param = Modifier.Param

module type S =
sig
  include Param

  exception Locked

  val resolve : Trie.path -> (data * tag) option
  val include_singleton : ?context_visible:context -> ?context_export:context -> Trie.path * (data * tag) -> unit
  val include_subtree : ?context_visible:context -> ?context_export:context -> Trie.path * (data, tag) Trie.t -> unit
  val import_subtree : ?context:context -> Trie.path * (data, tag) Trie.t -> unit
  val modify_visible : ?context:context -> hook Language.t -> unit
  val modify_export : ?context:context -> hook Language.t -> unit
  val export_visible : ?context:context -> hook Language.t -> unit
  val get_export : unit -> (data, tag) Trie.t

  val section : ?context_visible:context -> ?context_export:context -> Trie.path -> (unit -> 'a) -> 'a

  val run : ?export_prefix:Trie.bwd_path -> ?init_visible:(data, tag) Trie.t -> (unit -> 'a) -> (data, tag, hook, context) handler -> 'a
  val run_modifier : (unit -> 'a) -> (data, tag, hook, context) handler -> 'a
  val perform : (data, tag, hook, context) handler
  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
