(** Yuujinchou is an OCaml package of name modifiers. *)

(** The {!module:Trie} module implements mappings from paths to values that support efficient subtree operations. *)
module Trie : sig
  include module type of Trie
  (** @open *)

  module Untagged : module type of UntaggedTrie
end

(** The {!module:Language} module defines the language of modifiers. *)
module Language :
sig
  (** {1 Types} *)

  (** The abstract type of modifiers, parametrized by the type of hook labels. See {!val:hook} for hook labels.

      Construct terms using builders in {!module:Language} and execute them using {!val:Modifier.S.modify}. *)
  type 'hook t

  (** Checking equality. *)
  val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

  (** {1 Modifier Builders} *)

  (** {2 Basics} *)

  (** [any] keeps the content of the current tree. It is an error if the tree is empty (no name to match).
      To avoid the emptiness checking, use the identity modifier {!val:seq}[ []].
      This is equivalent to {!val:only}[ []]. *)
  val any : 'hook t

  (** [only path] keeps the subtree rooted at [path]. It is an error if the subtree was empty. *)
  val only : Trie.path -> 'hook t

  (** [in_ path m] runs the modifier [m] on the subtree rooted at [path]. Bindings outside the subtree are kept intact. For example, [in_ ["x"] ]{!val:any} will keep [y] (if existing), while {!val:only}[ ["x"]] will drop [y]. *)
  val in_ : Trie.path -> 'hook t -> 'hook t

  (** {2 Negation} *)

  (** [none] drops everything. It is an error if the tree was already empty (nothing to drop).
      To avid the emptiness checking, use the empty modifier {!val:union}[ []]. *)
  val none : 'hook t

  (** [except p] drops the subtree rooted at [p]. It is an error if there was nothing in the subtree. This is equivalent to {!val:in_}[ p ]{!val:none}. *)
  val except : Trie.path -> 'hook t

  (** {2 Renaming} *)

  (** [renaming path path'] relocates the subtree rooted at [path] to [path']. The existing bindings at [path'] (if any) will be dropped.
      It is an error if the subtree was empty (nothing to move). *)
  val renaming : Trie.path -> Trie.path -> 'hook t

  (** {2 Sequencing} *)

  (** [seq [m0; m1; m2; ...; mn]] runs the modifiers [m0], [m1], [m2], ..., [mn] in order.
      In particular, [seq []] is the identity modifier. *)
  val seq : 'hook t list -> 'hook t

  (** {2 Union} *)

  (** [union [m0; m1; m2; ...; mn]] calculates the union of the results of individual modifiers [m0], [m1], [m2], ..., [mn].
      In particular, [union []] is the empty modifier.
      The {!field:Modifier.shadow} effect will be performed to resolve name conflicts,
      with an intention for results of a modifier to shadow those of previous ones. *)
  val union : 'hook t list -> 'hook t

  (** {2 Custom Hooks} *)

  (** [hook h] applies the hook labelled [h] to the entire trie
      by performing the {!field:Modifier.hook} effect. *)
  val hook : 'hook -> 'hook t

  (** {2 Ugly Printing} *)

  (** [dump dump_hook m] dumps the internal representation of [m] for debugging,
      where [dump_hook] is the ugly printer for hook labels (see {!val:hook}). *)
  val dump : (Format.formatter -> 'hook -> unit) -> Format.formatter -> 'hook t -> unit [@@ocaml.toplevel_printer]
end

(** The {!module:Modifier} module implements the engine running the modifiers of type {!type:Language.t}. *)
module Modifier :
sig

  (** The type of effect handlers used in this module. *)
  type ('data, 'tag, 'hook, 'context) handler = {
    not_found : ?context:'context -> Trie.bwd_path -> unit;
    (** [not_found prefix] is called when the engine expects at least one binding within the subtree at [prefix] but could not find any. Modifiers such as {!val:Language.any}, {!val:Language.only}, {!val:Language.none}, and a few other modifiers expect at least one matching binding. For example, the modifier {!val:Language.except}[ ["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the modifier will trigger this effect with [prefix] being [Emp #< "x" #< "y"]. See {!type:Param.context} for the argument [context]. *)

    shadow : ?context:'context -> Trie.bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag;
    (** [shadow path x y] is called when item [y] is being assigned to [path] but [x] is already bound at [path]. Modifiers such as {!val:Language.renaming} and {!val:Language.union} could lead to bindings having the same name, and when that happens, this function is called to resolve the conflicting bindings. To implement silent shadowing, one can simply return item [y]. One can also employ a more sophisticated strategy to implement type-directed disambiguation. See {!type:Param.context} for the argument [context]. *)

    hook : ?context:'context -> Trie.bwd_path -> 'hook -> ('data, 'tag) Trie.t -> ('data, 'tag) Trie.t;
    (** [hook prefix id input] is called when processing the modifiers created by {!val:Language.hook}. When the engine encounters the modifier {!val:Language.hook}[ id] while handling the subtree [input] at [prefix], it will call [hook prefix id input] and replace the existing subtree [input] with the return value. See {!type:Param.context} for the argument [context]. *)
  }

  (** The parameters of an engine. *)
  module type Param =
  sig
    (** The type of data held by the bindings that will survive retagging. *)
    type data

    (** The type of tags attached to the bindings that can be efficiently reset. *)
    type tag

    (** The type of modifier hook labels. *)
    type hook

    (** The type of contexts for handlers to distinguish different function calls. *)
    type context
  end

  (** The signature of the engine. *)
  module type S =
  sig
    include Param
    (** @closed *)

    val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
    (** [modify modifier trie] runs the [modifier] on the [trie] and return the transformed trie.

        @param context The context sent to the effect handlers. If unspecified, effects come with {!constructor:None} as their context.
        @param prefix The prefix prepended to any path or prefix sent to the effect handlers. The default is the empty path ([Emp]). *)

    val run : (unit -> 'a) -> (data, tag, hook, context) handler -> 'a
    (** [run f h] runs the thunk [f], using [h] to handle modifier effects. See {!type:handler}. *)

    val perform : (data, tag, hook, context) handler
    (** A handler that reperforms the effects. It can also be used to manually trigger the effects;
        for example, [perform.not_found (Emp #< "a" #< "b")] will perform the [not_found] effect
        to be handled by the outer handler. *)
  end

  (** The functor to generate an engine. *)
  module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
end

(** The {!module:Scope} module implements the scoping effects based on {!module:Modifier}. *)
module Scope :
sig
  (** The type of effect handlers used in this module. *)
  type ('data, 'tag, 'hook, 'context) handler = ('data, 'tag, 'hook, 'context) Modifier.handler

  (** The parameters of scoping effects. *)
  module type Param = Modifier.Param

  (** The signature of scoping effects. *)
  module type S =
  sig
    include Param
    (** @closed *)

    exception Locked
    (** The exception [Locked] is raised when an operation on a scope starts before another operation on the same scope is finished.
        This could happen when the user, for example, calls {!val:modify_visible} and then calls {!val:modify_export} when handling the effects.

        The principle is that one should not access any scope in its intermediate states, including looking up a name via {!val:resolve}.
        Any attempt to do so will raise the exception [Locked].

        Note: {!val:section} only locks the parent scope; the child scope is initially unlocked.
    *)

    (** {1 Basics} *)

    val resolve : Trie.path -> (data * tag) option
    (** [resolve p] looks up the name [p] in the current scope
        and return the data associated with the binding. *)

    val include_singleton : ?context_visible:context -> ?context_export:context -> Trie.path * (data * tag) -> unit
    (** [include_singleton (p, x)] adds a new binding to both the visible
        and export namespaces, where the binding is associating the data [x] to the path [p].
        Conflicting names during the final merge will trigger the effect [shadow].

        @param context_visible The context attached to the modifier effects when manipulating the visible namespace.
        @param context_export The context attached to the modifier effects when manipulating the export namespace. *)

    val include_subtree : ?context_visible:context -> ?context_export:context -> Trie.path * (data, tag) Trie.t -> unit
    (** [include_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
        both the visible and export namespaces. Conflicting names during the final merge
        will trigger the effect [shadow].

        @param context_visible The context attached to the modifier effects when manipulating the visible namespace.
        @param context_export The context attached to the modifier effects when manipulating the export namespace. *)

    val import_subtree : ?context:context -> Trie.path * (data, tag) Trie.t -> unit
    (** [include_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
        the visible namespace (while keeping the export namespace intact).
        Conflicting names during the final merge will trigger the effect [Mod.Shadowing].

        @param context The context attached to the modifier effects. *)

    val modify_visible : ?context:context -> hook Language.t -> unit
    (** [modify_visible m] modifies the visible namespace by
        running the modifier [m] on it, using the internal modifier engine.

        @param context The context attached to the modifier effects. *)

    val modify_export : ?context:context -> hook Language.t -> unit
    (** [modify_visible m] modifies the export namespace by
        running the modifier [m] on it, using the internal modifier engine.

        @param context The context attached to the modifier effects. *)

    val export_visible : ?context:context -> hook Language.t -> unit
    (** [export_visible m] runs the modifier [m] on the visible namespace,
        and then merge the result into the export namespace.
        Conflicting names during the final merge will trigger the effect [Mod.Shadowing].

        @param context The context attached to the modifier effects. *)

    val get_export : unit -> (data, tag) Trie.t
    (** [get_export ()] returns the export namespace of the current scope. *)

    (** {1 Sections} *)

    val section : ?context_visible:context -> ?context_export:context -> Trie.path -> (unit -> 'a) -> 'a
    (** [section p f] starts a new scope and runs the thunk [f] within the scope.
        The child scope inherits the visible namespace from the parent, and its export namespace
        will be prefixed with [p] and merged into both the visible and export namespaces
        of the parent scope.

        @param context_visible The context attached to the modifier effects
        when merging the content of the section into its parent's visible namespace.
        @param context_export The context attached to the modifier effects
        when merging the content of the section into its parent's export namespace. *)

    (** {1 Runners} *)

    val run : ?export_prefix:Trie.bwd_path -> ?init_visible:(data, tag) Trie.t -> (unit -> 'a) -> (data, tag, hook, context) handler -> 'a
    (** [run f h] executes [f] that performs scoping effects, using [h] to handle internal
        modifier effects.

        @param export_prefix The additional global prefix prepended to the paths reported to effect handlers
        originating from export namespaces. The default is the empty path ([Emp]).
        This does not affect paths originating from visible namespaces.
        @param init_visible The initial visible namespace. The default is the empty trie. *)

    val run_modifier : (unit -> 'a) -> (data, tag, hook, context) handler -> 'a
    (** Execute the code and handles the internal modifier effects. This can be used to intercept
        or reperform those effects; for example, the following function silences the [shadow] effects.
        See also {!val:Modifier.S.run}.

        {[
          let silence_shadow f = run_modifier f {perform with shadow = fun ?context:_ _ _ y -> y}
        ]}

        Note that {!val:run} runs the code with a fresh empty scope,
        while {!val:run_modifier} remains in the current scope.
    *)

    val perform : (data, tag, hook, context) handler
    (** A handler that reperforms the internal modifier effects. See {!val:Modifier.S.perform}. *)

    val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
    (** Call the internal modifier engine directly on some trie. See {!val:Modifier.S.modify}. *)
  end

  module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
  (** The functor to generate a module for scoping effects. *)
end
