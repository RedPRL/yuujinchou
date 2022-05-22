type ('data, 'tag, 'hook, 'context) handler = {
  not_found : ?context:'context -> Trie.bwd_path -> unit;
  shadow : ?context:'context -> Trie.bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag;
  hook : ?context:'context -> Trie.bwd_path -> 'hook -> ('data, 'tag) Trie.t -> ('data, 'tag) Trie.t;
}

module type Param =
sig
  type data
  type tag
  type hook
  type context
end

module type S =
sig
  include Param

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  val run : (unit -> 'a) -> (data, tag, hook, context) handler -> 'a

  val perform : (data, tag, hook, context) handler
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
