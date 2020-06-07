open Pattern

type 'a error =
  | ReplacementNotUsed of 'a pattern
  | EmptyInverseJoin of 'a pattern

type 'a result_ = [ `NoMatch | `Matched of (path * 'a) list ]

val run : default:'a -> join:('a->'a->'a) -> meet:('a->'a->'a) -> 'a pattern -> path -> ('a result_, 'a error) result
val run_ : unit pattern -> path -> (unit result_, unit error) result

val check : 'a pattern -> (unit, 'a error) result

val pp_result_ : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a result_ -> unit
val pp_result : (Format.formatter -> 'a -> unit) -> Format.formatter -> ('a result_, 'a error) result -> unit
val pp_check_result : (Format.formatter -> 'a -> unit) -> Format.formatter -> (unit, 'a error) result -> unit
