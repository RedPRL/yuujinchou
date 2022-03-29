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

module Make (Param : sig type data type hook end) : S with type data = Param.data and type hook = Param.hook
