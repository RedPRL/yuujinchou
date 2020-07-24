type path = string list

type +'a t

val empty : 'a t

val is_empty : 'a t -> bool

val mk_root : 'a option -> 'a t

val prefix : path -> 'a t -> 'a t
(* val singleton : path * 'a -> 'a t *)
(* val root : 'a -> 'a t *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)

(* val find_subtree : path -> 'a t -> 'a t *)
(* val find_singleton : path -> 'a t -> 'a option *)

val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
val union_subtree : ('a -> 'a -> 'a) -> 'a t -> path * 'a t -> 'a t
(* val union_singleton : ('a -> 'a -> 'a) -> 'a t -> path * 'a -> 'a t *)

(* val update_subtree : path -> ('a t -> 'a t) -> 'a t -> 'a t *)
(* val update_singleton : path -> ('a option -> 'a option) -> 'a t -> 'a t *)

val detach_subtree : path -> 'a t -> 'a t * 'a t
val detach_singleton : path -> 'a t -> 'a option * 'a t

val to_seq : 'a t -> (path * 'a) Seq.t
val of_seq : ('a -> 'a -> 'a) -> (path * 'a) Seq.t -> 'a t

(* val map : ('a -> 'b) -> 'a t -> 'b t *)
(* val map_endo : ('a -> 'a) -> 'a t -> 'a t *)
(* val filter : ('a -> bool) -> 'a t -> 'a t *)
(* val filter_map : ('a -> 'b option) -> 'a t -> 'b t *)
val filter_map_endo : ('a -> 'a option) -> 'a t -> 'a t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
