module type Param = ModifierSigs.Param
module type Perform = ModifierSigs.Perform

module type S =
sig
  module Language : LanguageSigs.S

  module Param : Param
  open Param

  type not_found_handler = context option -> Trie.bwd_path -> unit
  type shadow_handler = context option -> Trie.bwd_path -> data * tag -> data * tag -> data * tag
  type hook_handler = context option -> Trie.bwd_path -> hook -> (data, tag) Trie.t -> (data, tag) Trie.t

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
      Conflicting names during the final merge will trigger the effect [Mod.Shadowing].

      When implementing an OCaml-like language, one can import content from other compilation units using [import_subtree].

      @param context_modifier The context of modifier effects when applying [modifier].
      @param context_visible The context of modifier effects when merging the subtree into the visible namespace.
      @param modifier The modifier applied to the subtree before importing it. The default value is {!val:Language.id}. *)

  val modify_visible : ?context_visible:context -> hook Language.t -> unit
  (** [modify_visible m] modifies the visible namespace by
      running the modifier [m] on it, using the internal modifier engine.

      When implementing an OCaml-like language, one can implement [open M] as follows:
      {[
        modify_visible Language.(union [seq []; only ["M"]])
      ]}

      When implementing an OCaml-like language, one can implement [include M] as follows:
      {[
        export_visible Language.(only ["M"]);
        modify_visible Language.(union [seq []; only ["M"]])
      ]}

      @param context The context of modifier effects. *)

  val modify_export : ?context_export:context -> hook Language.t -> unit
  (** [modify_visible m] modifies the export namespace by
      running the modifier [m] on it, using the internal modifier engine.

      @param context_export The context of modifier effects. *)

  val export_visible : ?context_modifier:context -> ?context_export:context -> hook Language.t -> unit
  (** [export_visible m] runs the modifier [m] on the visible namespace,
      and then merge the result into the export namespace.
      Conflicting names during the final merge will trigger the effect [Mod.Shadowing].

      This feature is useful for implementing a userspace [export] statement. It does not exist in OCaml-like languages.

      @param context_modifier The context of modifier effects when applying the modifier [m].
      @param context_export The context of modifier effects when merging the subtree into the export namespace. *)

  val get_export : unit -> (data, tag) Trie.t
  (** [get_export ()] returns the export namespace of the current scope.

      This is useful for obtaining all exported content when wrapping up a compilation unit. The {!val:section} function internally calls [get_export] when wrapping up a child scope, but an implementer is expected to call [get_export] for the outermost scope. The outermost scope is special because it is the interface of the entire compilation unit and its ending often triggers special handling code ({i e.g.,} caching). *)

  (** {1 Local Scopes and Sections} *)

  val section : ?context_modifier:context -> ?context_visible:context -> ?context_export:context -> ?modifier:hook Language.t -> Trie.path -> (unit -> 'a) -> 'a
  (** [section p f] starts a new scope and runs the thunk [f] within the scope.
      The child scope inherits the visible namespace from the parent, and its export namespace
      will be prefixed with [p] and merged into both the visible and export namespaces
      of the parent scope.

      A section is similar to a section in Coq or a module in Agda (but not a module in OCaml). This can be used to implement local bindings as well; a local binding is a private definition in a section. For example, in an OCaml-like languages augmented with sections,
      {v
let y = let x = 1 in x
      v}
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

  val run : ?not_found:not_found_handler -> ?shadow:shadow_handler -> ?hook:hook_handler ->
    ?export_prefix:Trie.bwd_path -> ?init_visible:(data, tag) Trie.t -> (unit -> 'a) -> 'a
  (** [run ~not_found ~shadow ~hook f] initializes a scope and executes the thunk [f], using [h] to handle modifier effects.

      @param not_found See {!val:Yuujinchou.Modifier.S.run}
      @param shadow See {!val:Yuujinchou.Modifier.S.run}
      @param hook See {!val:Yuujinchou.Modifier.S.run}
      @param export_prefix The additional global prefix prepended to the paths reported to effect handlers
      originating from export namespaces. The default is the empty path ([Emp]).
      This does not affect paths originating from visible namespaces.
      @param init_visible The initial visible namespace. The default is the empty trie. *)

  val try_with : ?not_found:not_found_handler -> ?shadow:shadow_handler -> ?hook:hook_handler -> (unit -> 'a) -> 'a
  (** Execute the code and handles the internal modifier effects.

      [try_with] is intended to be used within {!val:run} to intercept or reperform internal effects,
      while {!val:run} is intended to be at the top-level to set up the environment and handle all
      effects by itself. For example, the following function silences the [Mod.Shadow] effects, but the
      silencing function should be used within the dynamic scope of a {!val:run}.
      See also {!val:Yuujinchou.Modifier.S.try_with}.
      {[
        let silence_shadow f =
          try_with ~shadow:Silence.shadow f
      ]}

      A consequence of the semantic difference between {!val:run} and [try_with] is that
      {!val:run} starts a fresh empty scope while [try_with] stays in the current scope.
  *)

  module type Perform = Perform with module Param := Param
  module Perform : Perform
  module Silence : Perform
end
