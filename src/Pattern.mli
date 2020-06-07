(** The type of names. *)
type path = string list

(** The type of patterns, parametrized by the attribute type. *)
type 'a pattern =
  | PatWildcard (** Wildcard. *)
  | PatScope of path * path option * 'a pattern (** Scope renaming. *)
  | PatSeq of 'a pattern list (** Sequencing. *)
  | PatInv of 'a pattern (** Mode inversion. *)
  | PatJoin of 'a pattern list (** Join operator. *)
  | PatAttr of 'a * 'a pattern (** Attribute assignment. *)

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

(** Join of a list of elements. *)
val join : 'a pattern list -> 'a pattern

(** Meet of a list of elements. *)
val meet : 'a pattern list -> 'a pattern

(** The pattern that skips the specified name. *)
val skip : path -> 'a pattern

(** The pattern that skips any name with the specified prefix. *)
val skip_prefix : path -> 'a pattern

(** Pretty-printer for {!type:path}. *)
val pp_path : Format.formatter -> path -> unit

(** Pretty-printer for {!type:pattern}. *)
val pp_pattern : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a pattern -> unit
