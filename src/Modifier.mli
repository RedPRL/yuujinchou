type source = ..

module type Param =
sig
  type data
  type hook
end

module type S =
sig
  include Param

  type _ Effect.t +=
    | BindingNotFound : {source : source option; prefix : Trie.bwd_path} -> unit Effect.t
    | Shadowing : {source : source option; path : Trie.bwd_path; former : data; latter : data} -> data Effect.t
    | Hook : {source : source option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> data Trie.t Effect.t

  val exec : ?source:source -> ?prefix:Trie.bwd_path -> hook Language.modifier -> data Trie.t -> data Trie.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook
