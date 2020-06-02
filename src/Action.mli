open Pattern

type error =
  | ReplacementNotUsed of path * path
  | EmptyMeetOrNegatedJoin of pattern

type result = [ `NoMatch | `Matched of (path * exportability) list ]

val run : export:exportability -> pattern -> path -> (result, error) Result.t
