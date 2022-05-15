module type Param =
sig
  type data
  type hook
  type context
end

module type S =
sig
  include Param

  type _ Effect.t +=
    | BindingNotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {context : context option; path : Trie.bwd_path; former : data; latter : data} -> data Effect.t
    | Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> data Trie.t Effect.t

  val exec : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.modifier -> data Trie.t -> data Trie.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook and type context = P.context
