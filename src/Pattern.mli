type path = string list
type 'a pattern =
  | PatWildcard
  | PatScope of path * path option * 'a pattern
  | PatSeq of 'a pattern list
  | PatInv of 'a pattern
  | PatJoin of 'a pattern list
  | PatAttr of 'a * 'a pattern

val inv : 'a pattern -> 'a pattern
val wildcard : 'a pattern
val root : 'a pattern
val scope : path -> 'a pattern -> 'a pattern
val renaming_scope : path -> path -> 'a pattern -> 'a pattern
val seq : 'a pattern list -> 'a pattern
val none : 'a pattern
val any : 'a pattern
val id : path -> 'a pattern
val renaming : path -> path -> 'a pattern
val attr : 'a -> 'a pattern -> 'a pattern
val join : 'a pattern list -> 'a pattern
val meet : 'a pattern list -> 'a pattern

val pp_path : Format.formatter -> path -> unit
val pp_pattern : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a pattern -> unit
