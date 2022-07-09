(* See Yuujinchou.mli for documentation. *)
open ModifierSigs

module type Param = Param
module type S = ModifierSigs.S with module Language := Language

module Make (Param : Param) : S with module Param := Param
