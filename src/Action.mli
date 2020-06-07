open Pattern

(** Error type *)
type 'a error =
  | ReplacementNotUsed of 'a pattern
  | EmptyInverseJoin of 'a pattern

(** The result type of pattern matching *)
type 'a result_ = [
  | `NoMatch (** The pattern does not match the name *)
  | `Matched of (path * 'a) list (** The pattern matches the name, with a list of tagged new names after renaming *)
]

(** The pattern matching engine.

    @param default The default attribute for the engine to start with.
    @param join The join operator to resolve conflicting attributes.
    @param meet The meet operator to resolve conflicting attributes.
*)
val run : default:'a -> join:('a -> 'a -> 'a) -> meet:('a->'a->'a) -> 'a pattern -> path -> ('a result_, 'a error) result

(** The engine specialized to the [unit] type as the attribute type *)
val run_ : unit pattern -> path -> (unit result_, unit error) result

(** Check whether the pattern would lead to errors if run. *)
val check : 'a pattern -> (unit, 'a error) result

(** Pretty-printer for {!type:result_}. *)
val pp_result_ : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a result_ -> unit

(** Pretty-printer for results of pattern matching. *)
val pp_result : (Format.formatter -> 'a -> unit) -> Format.formatter -> ('a result_, 'a error) result -> unit

(** Pretty-printer for results checking patterns. *)
val pp_check_result : (Format.formatter -> 'a -> unit) -> Format.formatter -> (unit, 'a error) result -> unit
