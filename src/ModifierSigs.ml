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

  module type Perform = Perform with module Param := Param
  (** The signature of a module implementing all effect handlers for a modifier engine. *)

  module Perform : Perform
  (** The module that (re-)perform effects. *)

  module Silence : Perform
  (** The module that silence effects. *)

  type not_found_handler = context option -> Trie.bwd_path -> unit
  (** The type of a handler of the {!val:Modifier.S.module-Perform.not_found} effect. *)

  type shadow_handler = context option -> Trie.bwd_path -> data * tag -> data * tag -> data * tag
  (** The type of a handler of the {!val:Modifier.S.module-Perform.shadow} effect. *)

  type hook_handler = context option -> Trie.bwd_path -> hook -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** The type of a handler of the {!val:Modifier.S.module-Perform.hook} effect. *)

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** [modify modifier trie] runs the [modifier] on the [trie] and return the transformed trie.

      @param context The context sent to the effect handlers. If unspecified, effects come with {!constructor:None} as their context.
      @param prefix The prefix prepended to any path or prefix sent to the effect handlers. The default is the empty path ([Emp]). *)

  val run : ?not_found:not_found_handler -> ?shadow:shadow_handler -> ?hook:hook_handler -> (unit -> 'a) -> 'a
  (** [run f] initializes the engine and runs the thunk [f].

      @param not_found [not_found ctx prefix] is called when the engine expects at least one binding within the subtree at [prefix] but could not find any, where [ctx] is the context passed to {!val:modify}. Modifiers such as {!val:Language.all}, {!val:Language.only}, {!val:Language.none}, and a few other modifiers expect at least one matching binding. For example, the modifier {!val:Language.except}[ ["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the modifier will trigger this effect with [prefix] being [Emp #< "x" #< "y"]. The default handler directly returns the [()].
      @param shadow [shadow ctx path x y] is called when item [y] is being assigned to [path] but [x] is already bound at [path], where [ctx] is the context passed to {!val:modify}. Modifiers such as {!val:Language.renaming} and {!val:Language.union} could lead to bindings having the same name, and when that happens, this function is called to resolve the conflicting bindings. The default handler directly returns the [y], effectively silencing the effects.
      @param hook [hook prefix id input] is called when processing the modifiers created by {!val:Language.hook}, where [ctx] is the context passed to {!val:modify}. When the engine encounters the modifier {!val:Language.hook}[ id] while handling the subtree [input] at [prefix], it will call [hook prefix id input] and replace the existing subtree [input] with the return value. The default handler returns [input], effective skipping the hooks. *)

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
end
