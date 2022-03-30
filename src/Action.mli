module type S =
sig
  type data
  type hook
  type _ Effect.t +=
    | BindingNotFound : Trie.bwd_path -> unit Effect.t
    | Shadowing : Trie.bwd_path * data * data -> data Effect.t
    | Hook : hook * Trie.bwd_path * data Trie.t -> data Trie.t Effect.t

  val run : ?prefix:Trie.bwd_path -> hook Pattern.t -> data Trie.t -> data Trie.t
end

module type PARAM =
sig
  type data
  type hook
end

module Make (Param : PARAM) : S with type data = Param.data and type hook = Param.hook
