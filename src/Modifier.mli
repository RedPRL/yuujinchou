(* See Yuujinchou.mli for documentation. *)
module type Param = ModifierSigs.Param
module type Handler = ModifierSigs.Handler
module type S = ModifierSigs.S with module Language := Language

module Make (Param : Param) : S with module Param := Param
