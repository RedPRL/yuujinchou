type path = string list

type switch = [`Keep | `Hide]

type 'custom act =
  | A_switch of switch
  | A_custom of 'custom

type 'custom split =
  { mode : [`Subtree | `Node]
  ; prefix : path
  ; prefix_replacement : path option
  ; on_target : 'custom t
  ; on_others : 'custom t
  }

and 'custom t =
  | P_act of 'custom act
  | P_split of 'custom split
  | P_seq of 'custom t list
  | P_union of 'custom t list

val equal : ('custom -> 'custom -> bool) -> 'custom t -> 'custom t -> bool
val pp : (Format.formatter -> 'custom -> unit) -> Format.formatter -> 'custom t -> unit
val pp_path : Format.formatter -> path -> unit

val any : 'custom t
val root : 'custom t
val wildcard : 'custom t

val only : path -> 'custom t
val only_subtree : path -> 'custom t

val none : 'custom t
val except : path -> 'custom t
val except_subtree : path -> 'custom t
val in_subtree : path -> 'custom t -> 'custom t

val renaming : path -> path -> 'custom t
val renaming_subtree : path -> path -> 'custom t

val seq : 'custom t list -> 'custom t

val union : 'custom t list -> 'custom t

val custom : 'custom -> 'custom t
