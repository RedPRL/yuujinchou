type path = string list

val pp_path : Format.formatter -> path -> unit

type switch = [`Use | `Hide]

type 'hook act =
  | A_switch of switch
  | A_hook of 'hook

type 'hook split =
  { prefix : path
  ; prefix_replacement : path option
  ; on_target : 'hook t
  ; keep_others : bool
  }

and 'hook t =
  | P_act of 'hook act
  | P_split of 'hook split
  | P_seq of 'hook t list
  | P_union of 'hook t list

val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

val any : 'hook t

val only : path -> 'hook t

val none : 'hook t
val except : path -> 'hook t
val in_ : path -> 'hook t -> 'hook t

val renaming : path -> path -> 'hook t

val seq : 'hook t list -> 'hook t

val union : 'hook t list -> 'hook t

val hook : 'hook -> 'hook t
