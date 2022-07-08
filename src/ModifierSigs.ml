
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

module type Handler =
sig
  module P : Param

  val not_found : P.context option -> Trie.bwd_path -> unit
  (** [not_found ctx prefix] is called when the engine expects at least one binding within the subtree at [prefix] but could not find any, where [ctx] is the context passed to {!val:S.modify}. Modifiers such as {!val:Language.any}, {!val:Language.only}, {!val:Language.none}, and a few other modifiers expect at least one matching binding. For example, the modifier {!val:Language.except}[ ["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the modifier will trigger this effect with [prefix] being [Emp #< "x" #< "y"]. *)

  val shadow : P.context option -> Trie.bwd_path -> P.data * P.tag -> P.data * P.tag -> P.data * P.tag
  (** [shadow ctx path x y] is called when item [y] is being assigned to [path] but [x] is already bound at [path], where [ctx] is the context passed to {!val:S.modify}. Modifiers such as {!val:Language.renaming} and {!val:Language.union} could lead to bindings having the same name, and when that happens, this function is called to resolve the conflicting bindings. To implement silent shadowing, one can simply return item [y]. One can also employ a more sophisticated strategy to implement type-directed disambiguation. *)


  val hook : P.context option -> Trie.bwd_path -> P.hook -> (P.data, P.tag) Trie.t -> (P.data, P.tag) Trie.t
  (** [hook prefix id input] is called when processing the modifiers created by {!val:Language.hook}, where [ctx] is the context passed to {!val:S.modify}. When the engine encounters the modifier {!val:Language.hook}[ id] while handling the subtree [input] at [prefix], it will call [hook prefix id input] and replace the existing subtree [input] with the return value. *)

end

module type S =
sig
  module Language : LanguageSigs.S

  module P : Param
  open P
  (** @closed *)

  val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
  (** [modify modifier trie] runs the [modifier] on the [trie] and return the transformed trie.

      @param context The context sent to the effect handlers. If unspecified, effects come with {!constructor:None} as their context.
      @param prefix The prefix prepended to any path or prefix sent to the effect handlers. The default is the empty path ([Emp]). *)

  module Handle (H : Handler with module P := P) :
  sig
    val run : (unit -> 'a) -> 'a
    (** [run f h] initializes the engine and runs the thunk [f], using [h] to handle modifier effects. See {!type:handler}. *)

    val try_with : (unit -> 'a) -> 'a
    (** [try_with f h] runs the thunk [f], using [h] to handle the intercepted modifier effects. See {!type:handler}.

        Currently, [try_with] is an alias of {!val:run}, but [try_with] is intended to use within {!val:run} to intercept effects,
        while {!val:run} is intended to be at the outermost layer to handle effects. That is, the following is the expected program structure:
        {[
          run @@ fun () ->
          (* code *)
          try_with f
          (* more code *)
        ]}
    *)
  end

  module Perform : Handler with module P := P
  (** A handler that reperforms the effects. It can also be used to manually trigger the effects;
      for example, [Perform.not_found (Emp #< "a" #< "b")] will perform the [not_found] effect
      to be handled by the outer handler. *)
end
