module type Param =
sig
  type data
  type hook
  type caller
  val compare_data : data -> data -> int
end

module type S =
sig
  include Param

  module DataSet : Set.S with type elt = data

  type _ Effect.t +=
    | BindingNotFound : {caller : caller option; prefix : Trie.bwd_path} -> unit Effect.t
    | Hook : {caller : caller option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> DataSet.t Effect.t

  val exec : ?caller:caller -> ?prefix:Trie.bwd_path -> hook Language.selector -> data Trie.t -> DataSet.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook and type caller = P.caller
