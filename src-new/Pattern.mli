type path = string list

type 'a act =
  | ActCheckExistence of
      { if_existing : [`Keep | `Hide]
      ; if_absent : [`Ok | `Error]
      }
  | ActFilterMap of ('a -> 'a option)

type 'a pattern =
  | PatAct of 'a act
  | PatSplit of
      { mode : [`Subtree | `Node]
      ; prefix : path
      ; prefix_replacement : path option
      ; on_target : 'a pattern
      ; on_others : 'a pattern
      }
  | PatSeq of 'a pattern list
  | PatUnion of 'a pattern list

val equal : ('a -> 'a -> bool) -> 'a pattern -> 'a pattern -> bool
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a pattern -> unit

val none : 'a pattern
val any : 'a pattern
val wildcard : 'a pattern
val root : 'a pattern

val only : path -> 'a pattern
val prefix : path -> 'a pattern
val only_subtree : path -> 'a pattern -> 'a pattern

val except : path -> 'a pattern
val except_prefix : path -> 'a pattern
val update_subtree : path -> 'a pattern -> 'a pattern

val renaming : path -> path -> 'a pattern
val renaming_prefix : path -> path -> 'a pattern
val renaming_subtree : path -> path -> 'a pattern -> 'a pattern

val seq : 'a pattern list -> 'a pattern

val union : 'a pattern list -> 'a pattern

val filter_map : ('a -> 'a option) -> 'a pattern
