type path = string list

type 'a t

val empty : 'a t

val root : 'a -> 'a t

val prefix : path -> 'a t -> 'a t

val singleton : path * 'a -> 'a t

val find_opt : path -> 'a t -> 'a option

val find_subtree_opt : path -> 'a t -> 'a t

val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val union_subtree : ('a -> 'a -> 'a) -> 'a t -> path * 'a t -> 'a t

val union_singleton : ('a -> 'a -> 'a) -> 'a t -> path * 'a -> 'a t

val intersect : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val detach : path -> 'a t -> 'a t * 'a t

val to_seq : 'a t -> (path * 'a) Seq.t

val of_seq : ('a -> 'a -> 'a) -> (path * 'a) Seq.t -> 'a t
