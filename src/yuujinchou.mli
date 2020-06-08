module Pattern :
sig
  (** The type of names. *)
  type path = string list

  (** The type of patterns, parametrized by the attribute type. *)
  type 'a pattern

  (** Inverting the pattern. *)
  val inv : 'a pattern -> 'a pattern

  (** Wildcard pattern. *)
  val wildcard : 'a pattern

  (** The pattern that only matches the root. *)
  val root : 'a pattern

  (** Scoping a pattern. *)
  val scope : path -> 'a pattern -> 'a pattern

  (** Scoping a pattern and rename the prefix. *)
  val renaming_scope : path -> path -> 'a pattern -> 'a pattern

  (** Sequencing. *)
  val seq : 'a pattern list -> 'a pattern

  (** The pattern that matches nothing. *)
  val none : 'a pattern

  (** The pattern that matches any name. *)
  val any : 'a pattern

  (** The pattern that matches only the specified name. *)
  val id : path -> 'a pattern

  (** The pattern that matches only the specified name and replaces it. *)
  val renaming : path -> path -> 'a pattern

  (** The pattern that matches any name with the given prefix. *)
  val prefix : path -> 'a pattern

  (** The pattern that matches any name with the given prefix and replaces the prefix. *)
  val renaming_prefix : path -> path -> 'a pattern

  (** The pattern that assigns the default attribute. *)
  val attr : 'a -> 'a pattern -> 'a pattern

  (** Join of a list of patterns. *)
  val join : 'a pattern list -> 'a pattern

  (** Meet of a list of patterns. *)
  val meet : 'a pattern list -> 'a pattern

  (** The pattern that skips the specified name. *)
  val skip : path -> 'a pattern

  (** The pattern that skips any name with the specified prefix. *)
  val skip_prefix : path -> 'a pattern

  (** Pretty-printer for {!type:path}. *)
  val pp_path : Format.formatter -> path -> unit

  (** Pretty-printer for {!type:pattern}. *)
  val pp_pattern : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a pattern -> unit
end

module Action :
sig
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
end
