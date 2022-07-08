(* See Yuujinchou.mli for documentation. *)
open ModifierSigs

module type Param = Param
module type Handler = Handler
module type S = ModifierSigs.S with module Language := Language

module Make (P : Param) : S with module P := P
