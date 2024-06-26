module type Param = ModifierSigs.Param
module type Perform = ModifierSigs.Perform

module type S =
sig
  module Language : LanguageSigs.S

  module Param : Param
  open Param

  (** A scope inherently has two namespaces: a {i visible} namespace that dictates what's visible, and an {i export} namespace recording all the names that will be exported. *)

  (** {1 Types of Effect Handlers} *)

  type not_found_handler = context option -> Trie.bwd_path -> unit
  (** The type of a handler of the {!val:Modifier.S.module-Perform.not_found} effect. *)

  type shadow_handler = context option -> Trie.bwd_path -> data * tag -> data * tag -> data * tag
  (** The type of a handler of the {!val:Modifier.S.module-Perform.shadow} effect. *)

  type hook_handler = context option -> Trie.bwd_path -> hook -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** The type of a handler of the {!val:Modifier.S.module-Perform.hook} effect. *)

  (** {1 Exceptions} *)

  exception Locked
  (** The exception [Locked] is raised when an operation on a scope starts before another operation on the same scope is finished.
      This could happen when the user, for example, calls {!val:modify_visible} and then calls {!val:modify_export} when handling the effects.

      The principle is that one should not access any scope in its intermediate states, including looking up a name via {!val:resolve}.
      Any attempt to do so will raise the exception [Locked]; the exception [Locked] signals a serious programming error.

      Note: {!val:section} only locks the parent scope; the child scope is initially unlocked.
  *)

  (** {1 Name Resolution} *)

  val resolve : Trie.path -> (data * tag) option
  (** [resolve p] looks up the name [p] in the visible namespace and returns the data associated with the binding. *)

  (** {1 Inclusion of New Names}

      Inclusion affects both visible and export namespaces, just like [include] in OCaml. *)

  val include_singleton : ?context_visible:context -> ?context_export:context -> Trie.path * (data * tag) -> unit
  (** [include_singleton (p, x)] adds a new binding to both the visible and export namespaces, where the binding is associating the data [x] to the path [p].
      Conflicting names during the final merge will trigger the effect [shadow].
      [include_singleton (p, x)] is equivalent to [include_subtree Trie.(singleton (p, x))], but potentially more efficient.

      When implementing an OCaml-like language, this is how one can introduce a top-level definition [let p = x].

      @param context_visible The context of modifier effects when merging the subtree into the visible namespace.
      @param context_export The context of modifier effects when merging the subtree into the export namespace. *)

  val include_subtree : ?context_modifier:context -> ?context_visible:context -> ?context_export:context -> ?modifier:hook Language.t -> Trie.path * (data, tag) Trie.t -> unit
  (** [include_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
      both the visible and export namespaces. Conflicting names during the final merge
      will trigger the effect [shadow].

      This feature is useful for introducing multiple top-level definitions at once.

      @param context_modifier The context of modifier effects when applying [modifier].
      @param context_visible The context of modifier effects when merging the subtree into the visible namespace.
      @param context_export The context of modifier effects when merging the subtree into the export namespace.
      @param modifier The modifier applied to the subtree before importing it. The default value is {!val:Language.id}. *)

  (** {1 Importing of New Names}

      Importing affects only the visible namespace, just like [open] in OCaml. *)

  val import_singleton : ?context_visible:context -> Trie.path * (data * tag) -> unit
  (** [import_singleton (p, x)] adds a new binding to the visible namespace (while keeping the export namespace intact), where the binding is associating the data [x] to the path [p].
      Conflicting names during the final merge will trigger the effect [shadow].
      [import_singleton (p, x)] is equivalent to [import_subtree Trie.(singleton (p, x))], but potentially more efficient.

      When implementing an OCaml-like language, one can implement the local binding [let p = x in e] as follows:
      {[
        section [] @@ fun () ->
        import_singleton (p, x);
        (* code for handling the expression [e] *)
      ]}

      @param context_visible The context of modifier effects when merging the subtree into the visible namespace.
      @since 5.0.0 *)

  val import_subtree : ?context_modifier:context -> ?context_visible:context -> ?modifier:hook Language.t -> Trie.path * (data, tag) Trie.t -> unit
  (** [import_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
      the visible namespace (while keeping the export namespace intact).
      Conflicting names during the final merge will trigger the effect [shadow].

      When implementing an OCaml-like language, one can import content from other compilation units using [import_subtree].

      @param context_modifier The context of modifier effects when applying [modifier].
      @param context_visible The context of modifier effects when merging the subtree into the visible namespace.
      @param modifier The modifier applied to the subtree before importing it. The default value is {!val:Language.id}. *)

  (** {1 Modifying Namespaces} *)

  val modify_visible : ?context_visible:context -> hook Language.t -> unit
  (** [modify_visible m] modifies the visible namespace by
      running the modifier [m] on it, using the internal modifier engine.

      When implementing an OCaml-like language, one can implement [open M] as follows:
      {[
        modify_visible Language.(union [id; renaming ["M"] []])
      ]}

      When implementing an OCaml-like language, one can implement [include M] as follows:
      {[
        export_visible Language.(renaming ["M"] []);
        modify_visible Language.(union [id; renaming ["M"] []])
      ]}

      @param context The context of modifier effects. *)

  val modify_export : ?context_export:context -> hook Language.t -> unit
  (** [modify_export m] modifies the export namespace by
      running the modifier [m] on it, using the internal modifier engine.

      @param context_export The context of modifier effects. *)

  (** {1 Exporting Names} *)

  val export_visible : ?context_modifier:context -> ?context_export:context -> hook Language.t -> unit
  (** [export_visible m] runs the modifier [m] on the visible namespace,
      and then merge the result into the export namespace.
      Conflicting names during the final merge will trigger the effect [shadow].

      This feature is useful for implementing a userspace [export] statement. It does not exist in OCaml-like languages.

      @param context_modifier The context of modifier effects when applying the modifier [m].
      @param context_export The context of modifier effects when merging the subtree into the export namespace. *)

  val get_visible : unit -> (data, tag) Trie.t
  (** [get_visible ()] returns the visible namespace of the current scope.

      This is useful for obtaining all visible names for auto-completion. It can also be used for checking whether a shadowed definition is still visible under another name. (However, scanning the entire visible namespace is expensive and should probably be avoided.)

      @since 5.2.0 *)

  val get_export : unit -> (data, tag) Trie.t
  (** [get_export ()] returns the export namespace of the current scope.

      This is useful for obtaining all exported content when wrapping up a compilation unit. The {!val:section} function internally calls [get_export] when wrapping up a child scope, but an implementer is expected to call [get_export] for the outermost scope. The outermost scope is special because it is the interface of the entire compilation unit and its ending often triggers special handling code ({i e.g.,} caching of declared names for faster scope checking). *)

  (** {1 Sections} *)

  val section : ?context_modifier:context -> ?context_visible:context -> ?context_export:context -> ?modifier:hook Language.t -> Trie.path -> (unit -> 'a) -> 'a
  (** [section p f] starts a new scope and runs the thunk [f] within the scope.
      The child scope inherits the visible namespace from the parent, and its export namespace
      will be prefixed with [p] and merged into both the visible and export namespaces
      of the parent scope.

      A section is similar to a section in Coq or a module in Agda (but not a module in OCaml). This can be used to implement local bindings as well; a local binding is a private definition in a section. For example, in an OCaml-like languages augmented with sections,
      {[
        let y = let x = 1 in x
      ]}
      is equivalent to
      {v
section {
  private let x = 1
  let y = x
} // this section exports y but not x
      v}

      @param context_modifier The context of modifier effects when applying [modifier] to the content of the section before the merging.
      @param context_visible The context of modifier effects when merging the content of the section into its parent's visible namespace.
      @param context_export The context of modifier effects when merging the content of the section into its parent's export namespace.
      @param modifier The modifier applied to the content of the section before the merging. The default value is {!Language.id}. *)

  (** {1 Runners} *)

  module type Perform = Perform with module Param := Param
  (** The signature of a module implementing all effect handlers for a lexical scope. *)

  module Perform : Perform
  (** The handlers that (re-)perform effects. *)

  module Silence : Perform
  (** The handlers that silence effects. All the triggers actually do nothing. *)

  val run : ?not_found:not_found_handler -> ?shadow:shadow_handler -> ?hook:hook_handler ->
    ?export_prefix:Trie.bwd_path -> ?init_visible:(data, tag) Trie.t -> (unit -> 'a) -> 'a
  (** [run ~not_found ~shadow ~hook f] initializes a scope and executes the thunk [f], using [h] to handle modifier effects.

      @param not_found See {!val:Yuujinchou.Modifier.S.run} for the explanation of this argument.
      @param shadow See {!val:Yuujinchou.Modifier.S.run} for the explanation of this argument.
      @param hook See {!val:Yuujinchou.Modifier.S.run} for the explanation of this argument.
      @param export_prefix The additional global prefix prepended to the paths reported to effect handlers
      originating from export namespaces. The default is the empty path ([Emp]).
      This does not affect paths originating from visible namespaces.
      @param init_visible The initial visible namespace. The default is the empty namespace. *)

  val try_with : ?not_found:not_found_handler -> ?shadow:shadow_handler -> ?hook:hook_handler -> (unit -> 'a) -> 'a
  (** Execute the code and handles the internal modifier effects.

      [try_with] is intended to be used within {!val:run} to intercept or reperform internal effects,
      while {!val:run} is intended to be at the top-level to set up the environment and handle all
      effects by itself. For example, the following function silences the [shadow] effects, but the
      silencing function should be used within the dynamic scope of a {!val:run}.
      See also {!val:Yuujinchou.Modifier.S.try_with}.
      {[
        let silence_shadow f =
          try_with ~shadow:Silence.shadow f
      ]}

      A consequence of the semantic difference between {!val:run} and [try_with] is that
      {!val:run} starts a fresh empty scope while [try_with] stays in the current scope.
  *)

  (** {1 Debugging} *)

  val register_printer : ([ `NotFound of context option * Trie.bwd_path | `Shadow of context option * Trie.bwd_path * (data * tag) * (data * tag) | `Hook of context option * Trie.bwd_path * hook * (data, tag) Trie.t ] -> string option) -> unit
  (** [register_printer f] registers a printer [p] via {!val:Printexc.register_printer} to convert unhandled internal effects into strings for the OCaml runtime system to display. See {!val:Yuujinchou.Modifier.S.register_printer}.

      @since 5.1.0
  *)
end
