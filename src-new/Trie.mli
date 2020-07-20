type path = string list

type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val prefix : path -> 'a t -> 'a t
val singleton : path * 'a -> 'a t
val root : 'a -> 'a t
val mk_root : 'a option -> 'a t

val find_subtree : path -> 'a t -> 'a t
val find_singleton : path -> 'a t -> 'a option
val find_root : 'a t -> 'a option

val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
val union_subtree : ('a -> 'a -> 'a) -> 'a t -> path * 'a t -> 'a t
val union_singleton : ('a -> 'a -> 'a) -> 'a t -> path * 'a -> 'a t
val union_root : ('a -> 'a -> 'a) -> 'a t -> 'a -> 'a t

val update_subtree : path -> ('a t -> 'a t) -> 'a t -> 'a t
val update_singleton : path -> ('a option -> 'a option) -> 'a t -> 'a t
val update_root : ('a option -> 'a option) -> 'a t -> 'a t

val detach_subtree : path -> 'a t -> 'a t * 'a t
val detach_singleton : path -> 'a t -> 'a option * 'a t
val detach_root : 'a t -> 'a option * 'a t

val to_seq : 'a t -> (path * 'a) Seq.t
val of_seq : ('a -> 'a -> 'a) -> (path * 'a) Seq.t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val filter_map_err : ('a -> ('b option, 'c) result) -> 'a t -> ('b t, path * 'c) result

(*
module PhyEq :
sig
  val tree : 'a t -> 'a t -> bool
  val root : 'a t -> 'a t -> bool
  val children : 'a t -> 'a t -> bool
end
*)
