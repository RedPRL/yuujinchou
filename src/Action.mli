module type Param =
sig
  type data
  type hook
end

module type S =
sig
  include Param

  type _ Effect.t +=
    | BindingNotFound : Trie.bwd_path -> unit Effect.t
    | Shadowing : Trie.bwd_path * data * data -> data Effect.t
    | Hook : hook * Trie.bwd_path * data Trie.t -> data Trie.t Effect.t

  val run : ?prefix:Trie.bwd_path -> hook Pattern.t -> data Trie.t -> data Trie.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook
