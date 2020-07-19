type path = string list

type 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val root : 'a -> 'a t

val root_opt : 'a option -> 'a t

val prefix : path -> 'a t -> 'a t

val singleton : path * 'a -> 'a t

val find_opt : path -> 'a t -> 'a option

val find_subtree : path -> 'a t -> 'a t

val find_root_opt : 'a t -> 'a option

val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val union_subtree : ('a -> 'a -> 'a) -> 'a t -> path * 'a t -> 'a t

val union_singleton : ('a -> 'a -> 'a) -> 'a t -> path * 'a -> 'a t

val intersect : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

val detach_subtree : path -> 'a t -> 'a t * 'a t

val detach_singleton : path -> 'a t -> 'a t * 'a option

val detach_root : 'a t -> 'a t * 'a option

val update_subtree : path -> ('a t -> 'a t) -> 'a t -> 'a t

val update_singleton : path -> ('a option -> 'a option) -> 'a t -> 'a t

val update_root : ('a option -> 'a option) -> 'a t -> 'a t

val to_seq : 'a t -> (path * 'a) Seq.t

val of_seq : ('a -> 'a -> 'a) -> (path * 'a) Seq.t -> 'a t
