type path = string list

type switch = [`Keep | `Hide]

type 'custom act =
  | A_switch of switch
  | A_custom of 'custom

type ('a, 'custom) split =
  { mode : [`Subtree | `Node]
  ; prefix : path
  ; prefix_replacement : path option
  ; on_target : ('a, 'custom) t
  ; on_others : ('a, 'custom) t
  }

and ('a, 'custom) t =
  | P_act of 'custom act
  | P_split of ('a, 'custom) split
  | P_seq of ('a, 'custom) t list
  | P_union of ('a, 'custom) t list

val equal : ('a -> 'a -> bool) -> ('custom -> 'custom -> bool) -> ('a, 'custom) t -> ('a, 'custom) t -> bool
val pp : (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'custom -> unit) -> Format.formatter -> ('a, 'custom) t -> unit

val any : ('a, 'custom) t
val root : ('a, 'custom) t
val wildcard : ('a, 'custom) t

val only : path -> ('a, 'custom) t
val only_subtree : path -> ('a, 'custom) t

val none : ('a, 'custom) t
val except : path -> ('a, 'custom) t
val except_subtree : path -> ('a, 'custom) t
val in_subtree : path -> ('a, 'custom) t -> ('a, 'custom) t

val renaming : path -> path -> ('a, 'custom) t
val renaming_subtree : path -> path -> ('a, 'custom) t

val seq : ('a, 'custom) t list -> ('a, 'custom) t

val union : ('a, 'custom) t list -> ('a, 'custom) t

val custom : 'custom -> ('a, 'custom) t
