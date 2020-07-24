type path = string list

type switch = [`Keep | `Hide]

type 'a act =
  | ActSwitch of switch
  | ActFilterMap of ('a -> 'a option)

type 'a t =
  | PatAct of 'a act
  | PatSplit of
      { mode : [`Subtree | `Node]
      ; prefix : path
      ; prefix_replacement : path option
      ; on_target : 'a t
      ; on_others : 'a t
      }
  | PatSeq of 'a t list
  | PatUnion of 'a t list

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val any : 'a t
val root : 'a t
val wildcard : 'a t

val only : path -> 'a t
val only_subtree : path -> 'a t

val none : 'a t
val except : path -> 'a t
val except_subtree : path -> 'a t
val in_subtree : path -> 'a t -> 'a t

val renaming : path -> path -> 'a t
val renaming_subtree : path -> path -> 'a t

val seq : 'a t list -> 'a t

val union : 'a t list -> 'a t

val filter_map : ('a -> 'a option) -> 'a t
