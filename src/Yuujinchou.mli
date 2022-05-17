(**
   {b Yuujinchou} is an OCaml package of name modifiers.

   {1 Introduction}

   This library was motivated by the name modifiers in the "import" or "include" statements present in all practical programming languages.
   Here are a few examples of such statements but without modifiers:

   {v open import M -- Agda v}

   {v import foo # Python v}

   The ability to import content from other files helps organize code. However, it also poses a new challenge: how could programmers prevent imported content from shadowing existing content? For example, if we already have a function [test] in the current scope, maybe we do not wish to import another function also named [test]. To address this, many programming languages allow programmers to selectively hide or rename part of the imported content:

   {v
open import M renaming (a to b) public
-- (Agda) renaming a to b, and then re-exporting the content
   v}

   Another way to address this is to place the imported content under some namespace. For example, in Python,

   {v
import math # Python: the sqrt function is available as `math.sqrt`.
   v}

   Arguably, common designs of these hiding or renaming mechanisms are quite limited. The goal of the Yuujinchou library is to provide a compositional calculus of these modifiers of names. Currently, the library supports renaming, scopes, sequencing, unions, and custom hooks for extending the engine.

   {2 Namespaces and Modules}

   This package intends to treat a namespace as the shared prefix of a group of names; there is no standalone namespace [a], but a group of unrelated declarations that happen to have names sharing the prefix [a]. This design is different from many others that attempt to group a collection of bindings as a standalone module.

   There are fundamental differences between modules (records) and namespaces. A module (or a record) is typed, in the sense that there will be a single type assigned to it. The typedness enables considering an abstract module of a module type. Signatures and functors in Standard ML and OCaml follow this approach. On the other hand, a namespace is untyped, which forbids the notion of abstract namespaces but enables flexible operations such as direct injection of a definition into a namespace. It seems impossible to have a unified design that is typed and supports flexible manipulations. Thus, we (the authors) believe that equating these two will necessarily limit operators on namespaces, which is the case in Standard ML and OCaml. (The modules in Haskell and Agda are namespaces in the above discussion.) Another approach is to have separate notions of namespaces and modules/records, as in C++ and our proof assistant cooltt.
*)

(**
   {1 Using the Library}

   {2 Library Organization}

   The library code is split into five parts:
*)

(** The {!module:Trie} module implements mappings from paths to values that support efficient subtree operations. *)
module Trie : module type of Trie

(** The {!module:Language} module defines the language of modifiers and selectors. *)
module Language :
sig
  (** {1 Types} *)

  (** The type of generic modifiers, parametrized by the type of hook labels. *)
  type (!'hook, !'kind) t constraint 'kind = [< `Modifier | `Selector]

  (** The type of modifiers, parametrized by the type of hook labels. See {!val:hook}. *)
  type 'hook modifier = ('hook, [`Modifier]) t

  (** The type of selectors, parametrized by the type of hook labels.
      Selectors are specialized modifiers that use explicit, positive listing of names to
      produce sets of data. Only {!val:any}, {!val:only}, {!val:none}, {!val:union}, and {!val:hook} are allowed. *)
  type 'hook selector = ('hook, [`Selector]) t

  (**
     The modifier type is abstract---you should build a modifier using the following builders and execute it by {!val:Modifier.S.exec}.
  *)

  (** Checking equality. *)
  val equal : ('hook -> 'hook -> bool) -> ('hook, 'kind) t -> ('hook, 'kind) t -> bool

  (** {1 Modifier Builders} *)

  (** {2 Basics} *)

  (** [any] keeps the content of the current tree. It is an error if the tree is empty (no name to match).
      To avoid the emptiness checking, use the identity modifier {!val:seq}[ []].
      This is equivalent to {!val:only}[ []].

      While this could technically can be a selector for completeness (as a degenerate case of the {!val:only} selector),
      one should avoid using this selector that selects everything.
  *)
  val any : ('hook, 'kind) t

  (** [only path] keeps the subtree rooted at [path]. It is an error if the subtree was empty. *)
  val only : Trie.path -> ('hook, 'kind) t

  (** [in_ path m] runs the modifier [m] on the subtree rooted at [path]. Bindings outside the subtree are kept intact. For example, [in_ ["x"] ]{!val:any} will keep [y] (if existing), while {!val:only}[ ["x"]] will drop [y]. *)
  val in_ : Trie.path -> 'hook modifier -> 'hook modifier

  (** {2 Negation} *)

  (** [none] drops everything. It is an error if the tree was already empty (nothing to drop).
      To avid the emptiness checking, use the empty modifier {!val:union}[ []]. *)
  val none : ('hook, 'kind) t

  (** [except p] drops the subtree rooted at [p]. It is an error if there was nothing in the subtree. This is equivalent to {!val:in_}[ p ]{!val:none}. *)
  val except : Trie.path -> 'hook modifier

  (** {2 Renaming} *)

  (** [renaming path path'] relocates the subtree rooted at [path] to [path']. It is an error if the subtree was empty (nothing to move). *)
  val renaming : Trie.path -> Trie.path -> 'hook modifier

  (** {2 Sequencing} *)

  (** [seq [m0; m1; m2; ...; mn]] runs the modifiers [m0], [m1], [m2], ..., [mn] in order.
      In particular, [seq []] is the identity modifier. *)
  val seq : 'hook modifier list -> 'hook modifier

  (** {2 Union} *)

  (** [union [m0; m1; m2; ...; mn]] calculates the union of the results of individual modifiers [m0], [m1], [m2], ..., [mn].
      In particular, [union []] is the empty modifier. As a selector, it produces the union set of the results of sub-selectors. *)
  val union : ('hook, 'kind) t list -> ('hook, 'kind) t

  (** {2 Custom Hooks} *)

  (** [hook h] applies the hook labelled [h] to the entire trie. See {!module-type:Modifier.S} for the effect [Hook] that will be performed when processing this modifier.
      As a selector, it applies the hook labelled [h] to calculate the data set. *)
  val hook : 'hook -> ('hook, 'kind) t

  (** {2 Ugly Printing} *)

  (** [dump dump_hook m] dumps the internal representation of [m] for debugging,
      where [dump_hook] is the ugly printer for hook labels (see {!val:hook}). *)
  val dump : (Format.formatter -> 'hook -> unit) -> Format.formatter -> ('hook, 'kind) t -> unit
end

(** The {!module:Modifier} module implements the engine running the modifiers of type {!type:Language.modifier}. *)
module Modifier :
sig

  (** The parameters of an engine. *)
  module type Param =
  sig
    (** The type of data held by the bindings. *)
    type data

    (** The type of tags attached to the bindings. *)
    type tag

    (** The type of modifier hook labels. *)
    type hook

    (** The type of contexts. Each effect may come a context for the effect handler to identify the source of the effect. *)
    type context
  end

  (** The signature of the engine. *)
  module type S =
  sig
    (** @open *)
    include Param

    (** The effect [BindingNotFound {context; prefix}] means that the engine expected at least one binding within the subtree at [path], but could not find any. Modifiers such as {!val:Language.any}, {!val:Language.only}, {!val:Language.none}, and a few other modifiers expect at least one matching binding. For example, the modifier {!val:Language.except}[ ["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the modifier will trigger this effect with [prefix] being [Emp #< "x" #< "y"]. See {!type:context} for the argument [context]. *)
    type _ Effect.t += BindingNotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t

    (** The effect [Shadowing {context; path; former; latter}] indicates that two items, [former] and [latter], are both assigned to the same [path]. Modifiers such as {!val:Language.renaming} and {!val:Language.union} could lead to bindings having the same name, and when that happens, this effect is performed to resolve the conflicting bindings. The effect is continued with the resolution of [former] and [latter]. For example, to implement silent shadowing, one can continue it with the item [latter]. One can also employ a more sophisticated strategy to implement type-directed disambiguation. See {!type:context} for the argument [context]. *)
    type _ Effect.t += Shadowing : {context : context option; path : Trie.bwd_path; former : data * tag; latter : data * tag} -> (data * tag) Effect.t

    (** The effect [Hook {context; prefix; hook; input}] is triggered by modifiers created by {!val:Language.hook}. When the engine encounters the modifier generated by {!val:Language.hook}[ hook] while handling the subtree [input] at [prefix], it will perform the effect [Hook {context; path; hook; input}], which may be continued with the resulting trie. See {!type:context} for the argument [context]. *)
    type _ Effect.t += Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> (data, tag) Trie.t Effect.t

    (** [exec ~prefix modifier trie] runs the [modifier] on the [trie] and return the transformed trie. It can perform effects [BindingNotFound], [Shadowing], and [Hook].

        @param context The context attached to the performed modifier effects. If unspecified, effects come with {!constructor:None} as their context. Note that this does not affect the resulting trie but only the effects performed by the engine. A typical use case is for the same effect handler to know in what context the effect is performed.
        @param prefix The prefix prepended to any path or prefix in the effects, but in reverse. The default is the empty unit path ([Emp]).

        @return The new trie after the transformation.
    *)
    val exec : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.modifier -> (data, tag) Trie.t -> (data, tag) Trie.t
  end

  (** The functor to generate an engine. *)
  module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
end

(** The {!module:Selector} module implements the engine running the selectors of type {!type:Language.selector},
    which are specialized modifiers that only allow explicit positive listing of names. *)
module Selector :
sig

  (** The parameters of an engine. *)
  module type Param =
  sig
    include Modifier.Param
    (** @open *)

    (** The comparator for {!type:data}. *)
    val compare_data : data -> data -> int
  end

  (** The signature of the engine. *)
  module type S =
  sig
    (** @open *)
    include Param

    (** The types of data sets. *)
    module DataSet : Set.S with type elt = data

    (** The effect [BindingNotFound {context; prefix}] means that the engine expected at least one binding within the subtree at [path], but could not find any. The selector {!val:Language.only} expects at least one matching binding. For example, the selector {!val:Language.except}[ ["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the selector will trigger this effect with [prefix] being [Emp #< "x" #< "y"]. See {!type:context} for the argument [context]. *)
    type _ Effect.t += BindingNotFound : {context : context option; prefix : Trie.bwd_path} -> unit Effect.t

    (** The effect [Hook {context; prefix; hook; input}] is triggered by selectors created by {!val:Language.hook}. When the engine encounters the selector generated by {!val:Language.hook}[ hook] while handling the subtree [input] at [prefix], it will perform the effect [Hook {context; path; hook; input}], which may be continued with the resulting set of data. See {!type:context} for the argument [context]. *)
    type _ Effect.t += Hook : {context : context option; prefix : Trie.bwd_path; hook : hook; input : (data, tag) Trie.t} -> DataSet.t Effect.t

    (** [exec ~prefix modifier trie] runs the [modifier] on the [trie] and return the transformed trie. It can perform effects [BindingNotFound] and [Hook].

        @param context The context attached to the performed modifier effects. If unspecified, effects come with {!constructor:None} as their context. Note that this does not affect the resulting trie but only the effects performed by the engine. A typical use case is for the same effect handler to know in what context the effect is performed.
        @param prefix The prefix prepended to any path or prefix in the effects, but in reverse. The default is the empty unit path ([Emp]).

        @return The new trie after the transformation.
    *)
    val exec : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.selector -> (data, tag) Trie.t -> DataSet.t
  end

  (** The functor to generate an engine. *)
  module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
end

(** The {!module:Scope} module implements the scoping effects based on {!module:Modifier}. *)
module Scope :
sig

  (** The parameters of scoping effects. *)
  module type Param = Modifier.Param

  (** The signature of scoping effects. *)
  module type S =
  sig
    include Param
    (** @open *)

    exception Locked
    (** The exception [Locked] is raised when an operation on a scope is called
        before another operation on the same scope is finished.
        This could happen when the user calls one of the public functions (for example, {!val:modify_visible}), and then calls the same
        function or another one (for example, {!val:modify_export}) while handling the effects performed by the modifier engine
        in {!module:Mod}.

        The principle is that you should not access any scope in its intermediate states, including {!val:resolve},
        and any attempt to do so will raise the exception [Locked].

        Note: {!val:section} only locks the parent scope; the child scope is initially unlocked.
    *)

    module Mod : Modifier.S with type data = data and type tag = tag and type hook = hook and type context = context
    (** The modifier engine used by the scoping mechanism to run modifiers.
        Note that {!module:Modifier.Make} is generative, so it is crucial to handle
        effects defined in this module, not other instances of modifier engines. *)

    val resolve : Trie.path -> (data * tag) option
    (** [resolve p] looks up the name [p] in the current scope
        and return the data associated with the binding. *)

    val modify_visible : ?context:context -> hook Language.modifier -> unit
    (** [modify_visible m] modifies the visible namespace by
        running the modifier [m] on it, using {!val:Mod.exec}.

        @param context The context attached to the modifier effects. *)

    val modify_export : ?context:context -> hook Language.modifier -> unit
    (** [modify_visible m] modifies the export namespace by
        running the modifier [m] on it, using {!val:Mod.exec}.

        @param context The context attached to the modifier effects. *)

    val export_visible : ?context:context -> hook Language.modifier -> unit
    (** [export_visible m] runs the modifier on the visible namespace,
        using {!val:Mod.exec}, but then merge the result into the export namespace.
        Conflicting names during the final merge will trigger the effect [Mod.Shadowing].

        @param context The context attached to the modifier effects. *)

    val include_singleton : ?context_visible:context -> ?context_export:context -> Trie.path * (data * tag) -> unit
    (** [include_singleton (p, x)] adds a new binding to both the visible
        and export namespaces, where the binding is associating the data [x] to the path [p].
        Conflicting names during the final merge will trigger the effect [Mod.Shadowing].

        @param context_visible The context attached to the modifier effects when manipulating the visible namespace.
        @param context_export The context attached to the modifier effects when manipulating the export namespace. *)

    val include_subtree : ?context_visible:context -> ?context_export:context -> Trie.path * (data, tag) Trie.t -> unit
    (** [include_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
        both the visible and export namespaces. Conflicting names during the final merge
        will trigger the effect [Mod.Shadowing].

        @param context_visible The context attached to the modifier effects when manipulating the visible namespace.
        @param context_export The context attached to the modifier effects when manipulating the export namespace. *)

    val import_subtree : ?context:context -> Trie.path * (data, tag) Trie.t -> unit
    (** [include_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
        the visible namespace (while keeping the export namespace intact).
        Conflicting names during the final merge will trigger the effect [Mod.Shadowing].

        @param context The context attached to the modifier effects. *)

    val get_export : unit -> (data, tag) Trie.t
    (** [get_export ()] returns the export namespace of the current scope. *)

    val section : ?context_visible:context -> ?context_export:context -> Trie.path -> (unit -> 'a) -> 'a
    (** [section p f] starts a new scope and runs the thunk [f] within the scope.
        The child scope inherits the visible namespace from the parent, and its export namespace
        will be prefixed with [p] and merged into both the visible and export namespaces
        of the parent scope.

        @param context_visible The context attached to the modifier effects
        when merging the content of the section into its parent's visible namespace.
        @param context_export The context attached to the modifier effects
        when merging the content of the section into its parent's export namespace. *)

    val run : ?prefix:Trie.bwd_path -> (unit -> 'a) -> 'a
    (** Execute the code that performs scoping effects.

        @param prefix The additional global prefix prepended to the reported paths originating
        from export namespaces. The default is the empty unit path ([Emp]).
        This does not affect paths originating from visible namespaces. *)
  end

  module Make (P : Param) : S with type data = P.data and type tag = P.tag and type hook = P.hook and type context = P.context
  (** The functor to generate a module for scoping effects. *)
end

(**
   {2 Example Code}
*)
(* The example code should be in sync with README.markdown and test/Example.ml *)
(**
   {[
     (* The following shim does nothing for OCaml >= 5, but is needed for OCaml < 5. *)
     open Yuujinchou
     open Bwd

     (* A tiny language demonstrating some power of the Scope module. *)
     type modifier_cmd = Print
     type decl =
       (* declaration *)
       | Decl of Trie.path * int
       (* declaration, but supressing the shadowing warning *)
       | ShadowingDecl of Trie.path * int
       (* importing a trie after applying the modifier *)
       | Import of int Trie.untagged * modifier_cmd Language.modifier
       (* printing out all visible bindings *)
       | PrintVisible
       (* exporting a binding *)
       | Export of Trie.path
       (* section *)
       | Section of Trie.path * decl list
     type program = decl list

     (* Specialzed Scope module with Data.t *)
     module S = Scope.Make (struct
         type data = int
         type tag = [`Imported | `Local]
         type hook = modifier_cmd
         type context = [`Visible | `Export]
       end)

     (* Convert a backward path into a string for printing. *)
     let string_of_bwd_path =
       function
       | Emp -> "(root)"
       | path -> String.concat "." (BwdLabels.to_list path)

     (* Handle effects from running the modifiers. *)
     let handle_modifier_effects f =
       let open Effect.Deep in
       let string_of_context =
         function
         | Some `Visible -> " in the visible namespace"
         | Some `Export -> " in the export namespace"
         | None -> ""
       in
       let string_of_tag =
         function
         | `Imported -> " (imported)"
         | `Local -> " (local)"
       in
       try_with f ()
         { effc = fun (type a) (eff : a Effect.t) ->
               match eff with
               | S.Mod.BindingNotFound {context; prefix} -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 Format.printf "[Warning] Could not find any data within the subtree at %s%s.@."
                   (string_of_bwd_path prefix) (string_of_context context);
                 continue k ()
               | S.Mod.Shadowing {context; path; former; latter} -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 begin
                   Format.printf "[Warning] Data %i assigned at %s was shadowed by data %i%s.@."
                     (fst former) (string_of_bwd_path path) (fst latter) (string_of_context context);
                   continue k latter
                 end
               | S.Mod.Hook {context; prefix; hook = Print; input} -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 Format.printf "@[<v 2>[Info] Got the following bindings at %s%s:@;"
                   (string_of_bwd_path prefix) (string_of_context context);
                 Trie.iter
                   (fun path (data, tag) ->
                      Format.printf "%s => %i%s@;" (string_of_bwd_path path) data (string_of_tag tag))
                   input;
                 Format.printf "@]@.";
                 continue k input
               | _ -> None }

     (* Mute the shadowing effects. *)
     let silence_shadowing f =
       let open Effect.Deep in
       try_with f ()
         { effc = fun (type a) (eff : a Effect.t) ->
               match eff with
               | S.Mod.Shadowing {latter; _} -> Option.some @@
                 fun (k : (a, _) continuation) -> continue k latter
               | _ -> None }

     (* The interpreter *)
     let rec interpret_decl : decl -> unit =
       function
       | Decl (p, x) ->
         S.include_singleton ~context_visible:`Visible ~context_export:`Export (p, (x, `Local))
       | ShadowingDecl (p, x) ->
         silence_shadowing @@ fun () ->
         S.include_singleton (p, (x, `Local))
       | Import (t, m) ->
         let t = S.Mod.exec m (Trie.tag `Imported t) in
         S.import_subtree ([], t)
       | PrintVisible ->
         S.modify_visible (Language.hook Print)
       | Export p ->
         S.export_visible (Language.only p)
       | Section (p, sec) ->
         S.section p @@ fun () -> List.iter interpret_decl sec

     let interpret (prog : program) =
       handle_modifier_effects @@ fun () ->
       S.run (fun () -> List.iter interpret_decl prog)
   ]}

   {2 Implementing Features in Other Languages}

   This section shows how mechanisms in other languages can be implemented using this package. We use Haskell and Racket as examples.

   {3 Haskell}

   - Haskell syntax:
   {v
import Mod -- x is available an both x and Mod.x
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(union [any; renaming [] ["Mod"]])
   ]}

   - Haskell syntax:
   {v
import Mod (x,y)
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(union [only ["x"]; only ["y"]])
   ]}

   - Haskell syntax:
   {v
import qualified Mod
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.renaming [] ["Mod"]
   ]}

   - Haskell syntax:
   {v
import qualified Mod hiding (x,y)
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(seq [except ["x"]; except ["y"]; renaming [] ["Mod"]])
   ]}

   {3 Racket}

   - Racket syntax:
   {v
(require (only-in ... id0 [old-id1 new-id1]))
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(seq [...; union [only ["id0"]; seq [only ["old-id1"]; renaming ["old-id1"] ["new-id1"]]]])
   ]}

   - Racket syntax:
   {v
(require (except-in ... id0 id1]))
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(seq [...; except ["id0"]; except ["id1"]])
   ]}

   - Racket syntax:
   {v
(require (prefix-in p: ...))
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(seq [...; renaming [] ["p"]])
   ]}
   Note: Racket does not support hierarchical names, so the prefixing operator in Racket is directly prepending the prefix to the affected names.

   - Racket syntax:
   {v
(require (rename-in ... [old-id0 new-id0] [old-id1 new-id1]))
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(seq [...; renaming ["old-id0"] ["new-id0"]; renaming ["old-id1"] ["new-id1"]])
   ]}

   - Racket syntax:
   {v
(require (combine-in require-spec0 require-spec1 ...))
   v}
   Corresponding Yuujinchou modifier:
   {[
     Modifier.(union [require-spec0; require-spec1; ...])
   ]}

   {1 What is "Yuujinchou"?}

   "Yuujinchou" is the transliteration of "友人帳" in Japanese, which literally means "book of friends". It is a powerful notebook in the manga Natsume Yuujinchou (夏目友人帳) that collects many {e real names (真名)} of youkais (妖怪) (supernatural and spiritual monsters). These real names can be used to summon and control youkais, but the protagonist decided to return the names to their original owners. The plot is about meeting all kinds of youkais.

   This magical book will automatically turn to the page with the correct name when the protagonist pictures the youkai in his mind. This package is also about finding real names of youkais.

   Notes on the transliteration: "Yuujinchou" is in the Wāpuro style so that it uses only the English alphabet; otherwise, its Hepburn romanization would be "Yūjin-chō".
*)
