open Pattern

type error =
  | ReplacementNotUsed of path * path
  | EmptyMeetOrNegatedJoin of pattern

type result_ = [ `NoMatch | `Matched of (path * exportability) list ]

val run : exportability -> pattern -> path -> (result_, error) result

val pp_result_ : Format.formatter -> result_ -> unit
val pp_result : Format.formatter -> (result_, error) result -> unit
