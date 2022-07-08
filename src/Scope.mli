(* See Yuujinchou.mli for documentation. *)
open ScopeSigs

module type Param = Param
module type Handler = Handler
module type S = S with module Language := Language

module Make (P : Param) : S with module P := P
