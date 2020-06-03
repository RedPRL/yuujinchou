open Pattern

type error =
  | ReplacementNotUsed of pattern
  | EmptyMeetOrInverseJoin of pattern

type result_ = [ `NoMatch | `Matched of (path * exportability) list ]

val check : pattern -> (unit, error) result
val run : exportability -> pattern -> path -> (result_, error) result

val pp_result_ : Format.formatter -> result_ -> unit
val pp_result : Format.formatter -> (result_, error) result -> unit
val pp_check_result : Format.formatter -> (unit, error) result -> unit
