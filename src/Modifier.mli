(* See Yuujinchou.mli for documentation. *)

module type Param =
sig
  type data
  type tag
  type hook
  type context
end

module type Handler =
sig
  module P : Param
  val not_found : P.context option -> Trie.bwd_path -> unit
  val shadow : P.context option -> Trie.bwd_path -> P.data * P.tag -> P.data * P.tag -> P.data * P.tag
  val hook : P.context option -> Trie.bwd_path -> P.hook -> (P.data, P.tag) Trie.t -> (P.data, P.tag) Trie.t
end

module type S =
sig
  module P : Param
  open P

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t

  module Handle (H : Handler with module P := P) :
  sig
    val run : (unit -> 'a) -> 'a
    val try_with : (unit -> 'a) -> 'a
  end

  module Perform : Handler with module P := P
end

module Make (P : Param) : S with module P = P
