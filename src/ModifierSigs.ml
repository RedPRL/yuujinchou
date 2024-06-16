(** The parameters of an engine. *)
module type Param =
sig
  (** The type of data held by the bindings. The difference between data and tags is that the data will survive the efficient retagging. See {!val:Trie.retag}. *)
  type data

  (** The type of tags attached to the bindings. The difference between data and tags is that tags can be efficiently reset. See {!val:Trie.retag}. *)
  type tag

  (** The type of modifier hook labels. This is for extending the modifier language. *)
  type hook

  (** The type of contexts passed to each call of {!val:Modifier.S.modify} for the effect handler to distinguish different function calls. *)
  type context
end

module type Perform =
sig
  module Param : Param
  open Param

  val not_found : context option -> Trie.bwd_path -> unit
  (** Manually trigger the [not_found] effect. *)

  val shadow : context option -> Trie.bwd_path -> data * tag -> data * tag -> data * tag
  (** Manually trigger the [shadow] effect. *)

  val hook : context option -> Trie.bwd_path -> hook -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** Manually trigger the [hook] effect. *)

end

module type S =
sig
  module Language : LanguageSigs.S

  module Param : Param
  open Param

  (** {1 Types of Effect Handlers} *)

  type not_found_handler = context option -> Trie.bwd_path -> unit
  (** The type of a handler of the {!val:Modifier.S.module-Perform.not_found} effect. *)

  type shadow_handler = context option -> Trie.bwd_path -> data * tag -> data * tag -> data * tag
  (** The type of a handler of the {!val:Modifier.S.module-Perform.shadow} effect. *)

  type hook_handler = context option -> Trie.bwd_path -> hook -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** The type of a handler of the {!val:Modifier.S.module-Perform.hook} effect. *)

  (** {1 The Modifier Engine} *)

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** [modify modifier trie] runs the [modifier] on the [trie] and return the transformed trie.

      @param context The context sent to the effect handlers. If unspecified, effects come with {!constructor:None} as their context.
      @param prefix The prefix prepended to any path or prefix sent to the effect handlers. The default is the empty path ([Emp]). *)

  (** {1 Runners} *)

  module type Perform = Perform with module Param := Param
  (** The signature of a module implementing all effect handlers for a modifier engine. *)

  module Perform : Perform
  (** The handlers that (re-)perform effects. *)

  module Silence : Perform
  (** The handlers that silence effects. All the triggers actually do nothing. *)

  val run : ?not_found:not_found_handler -> ?shadow:shadow_handler -> ?hook:hook_handler -> (unit -> 'a) -> 'a
  (** [run f] initializes the engine and runs the thunk [f].

      @param not_found [not_found ctx prefix] is called when the engine expects at least one binding within the subtree at [prefix] but could not find any, where [ctx] is the context passed to {!val:modify}. Modifiers such as {!val:Language.all}, {!val:Language.only}, {!val:Language.none}, and a few other modifiers expect at least one matching binding. For example, the modifier {!val:Language.except}[ ["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the modifier will trigger this effect with [prefix] being [Emp #< "x" #< "y"]. The default handler directly returns the value [()], effectively ignoring the warning.
      @param shadow [shadow ctx path x y] is called when item [y] is being assigned to [path] but [x] is already bound at [path], where [ctx] is the context passed to {!val:modify}. Modifiers created by {!val:Language.union} could lead to multiple bindings having the same name, and when that happens, this function is called to resolve the conflicting bindings. The default handler directly returns the [y] (the later binding), effectively shadowing the earlier binding [x] silently.
      @param hook [hook prefix id input] is called when processing modifiers created by {!val:Language.hook}, where [ctx] is the context passed to {!val:modify}. When the engine encounters the modifier {!val:Language.hook}[ id] while handling the subtree [input] at [prefix], it will call [hook prefix id input] and replace the existing subtree [input] with the return value. The default handler returns [input], which is effectively a no-op. *)

  val try_with : ?not_found:not_found_handler -> ?shadow:shadow_handler -> ?hook:hook_handler -> (unit -> 'a) -> 'a
  (** [try_with f] runs the thunk [f] and intercepts modifier effects. See the documentation of {!val:run} for the meaning of the optional effect interceptors; the difference is that the default interceptors reperform the intercepted modifier effects instead of silencing them.

      [try_with] is intended to be used within {!val:run} to intercept or reperform effects, while {!val:run} is intended to be at the top-level to set up the environment and handle effects by itself. That is, the following is the expected program structure:
      {[
        run ~not_found ~shadow ~hook @@ fun () ->
        (* code *)
        try_with ~not_found @@ fun () ->
        (* more code *)
        try_with ~shadow @@ fun () ->
        (* even more code *)
      ]}
  *)

  (** {1 Re-exposed Union Functions for Tries} *)

  val union : ?context:context -> ?prefix:Trie.bwd_path -> (data, tag) Trie.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** Re-exposed {!val:Trie.union} with a merger that uses the [shadow] effect handler to resolve name conflicts. [union t1 t2] merges two tries [t1] and [t2]. If both tries have a binding at the same path [p], it will trigger the effect [shadow context p x y] to reconcile the values [x] from [t1] and [y] from [t2] that are both bound at the [path].

      @param context The context sent to the [shadow] effect handler. If unspecified, effects come with {!constructor:None} as their context.
      @param prefix The prefix prepended to any path or prefix sent to the [shadow] effect handler. The default is the empty path ([Emp]). *)

  val union_subtree : ?context:context -> ?prefix:Trie.bwd_path -> (data, tag) Trie.t -> Trie.path * (data, tag) Trie.t -> (data, tag) Trie.t
  (** Re-exposed {!val:Trie.union_subtree} with a merger that uses the [shadow] effect handler to resolve name conflicts. [union_subtree t1 (path, t2)] is equivalent to {!val:union}[ t1 (Trie.prefix path t2)], but potentially more efficient.

      @param context The context sent to the [shadow] effect handler. If unspecified, effects come with {!constructor:None} as their context.
      @param prefix The prefix prepended to any path or prefix sent to the [shadow] effect handler. The default is the empty path ([Emp]). *)

  val union_singleton : ?context:context -> ?prefix:Trie.bwd_path -> (data, tag) Trie.t -> Trie.path * (data * tag) -> (data, tag) Trie.t
  (** Re-exposed {!val:Trie.union_singleton} with a merger that uses the [shadow] effect handler to resolve name conflicts. [union_singleton t binding] is equivalent to {!val:union}[ t1 (Trie.singleton binding)], but potentially more efficient.

      @param context The context sent to the [shadow] effect handler. If unspecified, effects come with {!constructor:None} as their context.
      @param prefix The prefix prepended to any path or prefix sent to the [shadow] effect handler. The default is the empty path ([Emp]). *)

  val union_root : ?context:context -> ?prefix:Trie.bwd_path -> (data, tag) Trie.t -> data * tag -> (data, tag) Trie.t
  (** Re-exposed {!val:Trie.union_root} with a merger that uses the [shadow] effect handler to resolve name conflicts. [union_root t r] is equivalent to {!val:union_singleton}[ t ([], r)], but potentially more efficient.

      @param context The context sent to the [shadow] effect handler. If unspecified, effects come with {!constructor:None} as their context.
      @param prefix The prefix prepended to any path or prefix sent to the [shadow] effect handler. The default is the empty path ([Emp]). *)

  (** {1 Debugging} *)

  val register_printer : ([ `NotFound of context option * Trie.bwd_path | `Shadow of context option * Trie.bwd_path * (data * tag) * (data * tag) | `Hook of context option * Trie.bwd_path * hook * (data, tag) Trie.t ] -> string option) -> unit
  (** [register_printer p] registers a printer [p] via {!val:Printexc.register_printer} to convert unhandled internal effects into strings for the OCaml runtime system to display. Ideally, all internal effects should have been handled by {!val:run} and there is no need to use this function, but when it is not the case, this function can be helpful for debugging. The functor {!module:Modifier.Make} always registers a simple printer to suggest using {!val:run}, but you can register new ones to override it. The return type of the printer [p] should return [Some s] where [s] is the resulting string, or [None] if it chooses not to convert a particular effect. The registered printers are tried in reverse order until one of them returns [Some s] for some [s]; that is, the last registered printer is tried first. Note that this function is a wrapper of {!val:Printexc.register_printer} and all the registered printers (via this function or {!val:Printexc.register_printer}) are put into the same list.

      The input type of the printer [p] is a variant representation of all internal effects used in this module. They correspond to the three effect triggers by the functions in {!module:Perform}. More precisely,
      - [`NotFound (ctx, prefix)] corresponds to the effect triggered by [Perform.not_found ctx prefix]; and
      - [`Shadow (ctx, path, x, y)] corresponds to [Perform.shadow ctx path x y]; and
      - [`Hook (ctx, prefix, id, input)] corresponds to [Perform.hook ctx prefix id input].

      See also the documentation of {!val:run} for a detailed explanation of these effects.

      @since 5.1.0
  *)
end
