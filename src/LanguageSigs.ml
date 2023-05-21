module type S =
sig
  (** {1 Types} *)

  (** The abstract type of modifiers, parametrized by the type of hook labels. See {!val:hook} for hook labels. One can largely ignore hook labels unless {!val:hook} is used.

      The idea is to construct a modifier using builders in {!module:Language} and then execute it using {!val:Modifier.S.modify}. *)
  type 'hook t

  (** Checking equality. *)
  val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

  (** {1 Modifier Builders} *)

  (** {2 Basics} *)

  (** [all] keeps the content of the current tree.
      The {!val:Modifier.S.module-Perform.not_found} effect will be performed if the tree is empty (nothing to keep).
      To avoid the emptiness checking, use the identity modifier {!val:id}.
      This is equivalent to {!val:only}[ []]. *)
  val all : 'hook t

  (** [id] is {!val:seq}[ []], which keeps the content of the current tree.
      This is different from {!val:all} because, unlike {!val:all}, [id] does not check whether the tree is empty.

      @since 5.0.0 *)
  val id : 'hook t

  (** [only path] keeps the subtree rooted at [path]. Bindings outside the subtree are dropped.
      The {!val:Modifier.S.module-Perform.not_found} effect will be performed if the subtree is empty (nothing to keep). *)
  val only : Trie.path -> 'hook t

  (** [in_ path m] runs the modifier [m] on the subtree rooted at [path]. Bindings outside the subtree are kept intact.

      For example, suppose we have two bindings [x.a] and [y]. [in_ ["x"] ]{!val:all} will keep both [x.a] and [y], while {!val:only}[ ["x"]] will keep [x.y] but drop [y]. *)
  val in_ : Trie.path -> 'hook t -> 'hook t

  (** {2 Negation} *)

  (** [none] drops everything.
      The {!val:Modifier.S.module-Perform.not_found} effect will be performed if the tree was already empty (nothing to drop).
      To avoid the emptiness checking, use the empty modifier {!val:union}[ []]. *)
  val none : 'hook t

  (** [except p] drops the subtree rooted at [p].
      The {!val:Modifier.S.module-Perform.not_found} effect will be performed if the subtree was already empty (nothing to drop).
      This is equivalent to {!val:in_}[ p ]{!val:none}. *)
  val except : Trie.path -> 'hook t

  (** {2 Renaming} *)

  (** [renaming path path'] relocates the subtree rooted at [path] to [path']. The existing bindings with the prefix [path'] (if any) will be dropped.
      The {!val:Modifier.S.module-Perform.not_found} effect will be performed if the subtree was empty (nothing to move). *)
  val renaming : Trie.path -> Trie.path -> 'hook t

  (** {2 Sequencing} *)

  (** [seq [m0; m1; m2; ...; mn]] runs the modifiers [m0], [m1], [m2], ..., [mn] in order.
      In particular, [seq []] is the identity modifier {!val:id}. *)
  val seq : 'hook t list -> 'hook t

  (** {2 Union} *)

  (** [union [m0; m1; m2; ...; mn]] calculates the union of the results of individual modifiers [m0], [m1], [m2], ..., [mn].

      In particular, [union []] is the empty modifier; unlike {!val:none}, [union []] will not check whether the tree was already empty.

      The {!val:Modifier.S.module-Perform.shadow} effect will be performed to resolve name conflicts while calculating the union,
      assuming that the results from a later modifier are intended to shadow the results from an earlier modifier. *)
  val union : 'hook t list -> 'hook t

  (** {2 Custom Hooks} *)

  (** [hook h] applies the hook labelled [h] to the entire tree by performing the {!val:Modifier.S.module-Perform.hook} effect. *)
  val hook : 'hook -> 'hook t

  (** {2 Ugly Printing} *)

  (** [dump dump_hook m] dumps the internal representation of [m] for debugging,
      where [dump_hook] is the ugly printer for hook labels (see {!val:hook}). *)
  val dump : (Format.formatter -> 'hook -> unit) -> Format.formatter -> 'hook t -> unit [@@ocaml.toplevel_printer]
end
