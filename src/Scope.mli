(* See Yuujinchou.mli for documentation. *)
module type Param = ScopeSigs.Param
module type Handler = ScopeSigs.Handler
module type S = ScopeSigs.S with module Language := Language
module Make (Param : Param) (Modifier : Modifier.S with module Param := Param)
  : S with module Param := Param
