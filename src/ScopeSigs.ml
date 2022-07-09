module type Param = ModifierSigs.Param
module type Handler = ModifierSigs.Handler

module type S =
sig
  module Language : LanguageSigs.S

  module Param : Param
  open Param

  module type Handler = Handler with module Param := Param

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

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** Call the internal modifier engine directly on some trie. See {!val:Modifier.S.modify}.

      This will not lock the current scope. *)

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


  module Perform : Handler
  (** A handler that reperforms the internal modifier effects. See {!module:Modifier.S.Perform}. *)

  (** {1 Runners} *)

  module Run (H : Handler) :
  sig
    val run : ?export_prefix:Trie.bwd_path -> ?init_visible:(data, tag) Trie.t -> (unit -> 'a) -> 'a
    (** [run f h] initializes a scope and executes the thunk [f], using [h] to handle modifier effects.

        @param export_prefix The additional global prefix prepended to the paths reported to effect handlers
        originating from export namespaces. The default is the empty path ([Emp]).
        This does not affect paths originating from visible namespaces.
        @param init_visible The initial visible namespace. The default is the empty trie. *)

    val try_with : (unit -> 'a) -> 'a
    (** Execute the code and handles the internal modifier effects. This can be used to intercept
        or reperform those effects; for example, the following function silences the [shadow] effects.
        See also {!val:Modifier.S.Run.try_with}.

        {[
          module H =
          struct
            include Perform
            let shadow _ _ _ y = y
          end

          let silence_shadow f = let module R = Run (H) in R.try_with f
        ]}

        Note that {!val:run} starts a fresh empty scope while [try_with] remains in the current scope.
    *)
  end

end
