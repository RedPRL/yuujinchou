type path = string list

type 'hook t =
  | P_only of path
  | P_except of path
  | P_in of path * 'hook t
  | P_renaming of path * path
  | P_seq of 'hook t list
  | P_union of 'hook t list
  | P_hook of 'hook

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
