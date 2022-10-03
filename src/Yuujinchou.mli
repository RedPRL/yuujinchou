(** yuujinchou is an OCaml package of name modifiers. *)

(** The {!module:Trie} module implements mappings from paths to values that support efficient subtree operations. *)
module Trie : sig
  include module type of Trie
  (** @open *)

  module Untagged : module type of UntaggedTrie
end

(** The {!module:Language} module defines the language of modifiers. *)
module Language : LanguageSigs.S

(** The {!module:Modifier} module implements the engine running the modifiers of type {!type:Language.t}. *)
module Modifier :
sig
  (** The parameters of this module. *)
  module type Param = ModifierSigs.Param

  (** The handler module type. *)
  module type Perform = ModifierSigs.Perform

  (** The signature of the engine. *)
  module type S = ModifierSigs.S with module Language := Language

  (** The functor to generate an engine. *)
  module Make (Param : ModifierSigs.Param) : S with module Param := Param
end

(** The {!module:Scope} module implements lexical scoping based on {!module:Modifier}. *)
module Scope :
sig
  (** The parameters of scoping effects. *)
  module type Param = ScopeSigs.Param

  (** The handler module type. *)
  module type Perform = ModifierSigs.Perform

  (** The signature of scoping effects. *)
  module type S = ScopeSigs.S with module Language := Language

  (** The functor to generate a module for scoping effects. *)
  module Make (Param : Param) (Modifier : Modifier.S with module Param := Param) : S with module Param := Param
end
