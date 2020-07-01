open Pattern

type 'a error =
  | ReplacementNotUsed of 'a pattern
  | EmptyMeet of 'a pattern

type 'a compiled_pattern

val compile : join:('a -> 'a -> 'a) -> meet:('a->'a->'a) -> 'a pattern -> ('a compiled_pattern, 'a error) result
val compile_ : unit pattern -> (unit compiled_pattern, unit error) result

type 'a matching_result = [
  | `NoMatch
  | `Matched of (path * 'a) list
]

val run : 'a compiled_pattern -> default:'a -> path -> 'a matching_result
val run_ : unit compiled_pattern -> path -> unit matching_result

val pp_error : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a error -> unit
val pp_matching_result : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a matching_result -> unit
