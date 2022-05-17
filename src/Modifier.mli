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

  type _ Effect.t +=
    | BindingNotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t
    | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t

  val exec : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
end

module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
