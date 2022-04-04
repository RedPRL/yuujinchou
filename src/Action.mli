open Eff.StdlibShim

module type Param =
sig
  type data
  type hook
end

module type S =
sig
  include Param

  type source = ..

  type _ Effect.t +=
    | BindingNotFound : source option * Trie.bwd_path -> unit Effect.t
    | Shadowing : source option * Trie.bwd_path * data * data -> data Effect.t
    | Hook : source option * Trie.bwd_path * hook * data Trie.t -> data Trie.t Effect.t

  val run : ?source:source -> ?prefix:Trie.bwd_path -> hook Pattern.t -> data Trie.t -> data Trie.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook
