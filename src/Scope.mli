(* See Yuujinchou.mli for documentation. *)

module type Param = Modifier.Param
module type Handler = Modifier.Handler

module type S =
sig
  module P : Param
  open P

  exception Locked

  val resolve : Trie.path -> (data * tag) option
  val include_singleton : ?context_visible:context -> ?context_export:context -> Trie.path * (data * tag) -> unit
  val include_subtree : ?context_visible:context -> ?context_export:context -> Trie.path * (data, tag) Trie.t -> unit
  val import_subtree : ?context:context -> Trie.path * (data, tag) Trie.t -> unit
  val modify_visible : ?context:context -> hook Language.t -> unit
  val modify_export : ?context:context -> hook Language.t -> unit
  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  val export_visible : ?context:context -> hook Language.t -> unit
  val get_export : unit -> (data, tag) Trie.t

  val section : ?context_visible:context -> ?context_export:context -> Trie.path -> (unit -> 'a) -> 'a

  module Handle (H : Handler with module P := P) :
  sig
    val run : ?export_prefix:Trie.bwd_path -> ?init_visible:(data, tag) Trie.t -> (unit -> 'a) -> 'a
    val try_with : (unit -> 'a) -> 'a
  end

  module Perform : Handler with module P := P
end

module Make (P : Param) : S with module P = P
