(* See Yuujinchou.mli for documentation. *)
open ScopeSigs

module type Param = Param
module type S = S with module Language := Language
module Make (Param : Param) : S with module Param := Param
