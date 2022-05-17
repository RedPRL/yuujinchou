module type Param =
sig
  type data
  type tag
  type hook
  type context
  val compare_data : data -> data -> int
end

module type S =
sig
  include Param

  module DataSet : Set.S with type elt = data

  type _ Effect.t +=
    | BindingNotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
    | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> DataSet.t Effect.t

  val exec : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.selector -> (data, tag) Trie.t -> DataSet.t
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
