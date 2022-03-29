module type S =
sig
  type data
  type hook
  type _ Effect.t +=
    | BindingNotFound : Pattern.path -> unit Effect.t
    | Shadowing : Pattern.path * data * data -> data Effect.t
    | Hook : hook * Pattern.path * data Trie.t -> data Trie.t Effect.t

  val run : ?rev_prefix:Pattern.path -> hook Pattern.t -> data Trie.t -> data Trie.t
end

module Make (Param : sig type data type hook end) : S with type data = Param.data and type hook = Param.hook
