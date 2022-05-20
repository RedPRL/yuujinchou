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

   The library code is split into four parts:
*)

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

  (** [renaming path path'] relocates the subtree rooted at [path] to [path']. It is an error if the subtree was empty (nothing to move). *)
  val renaming : Trie.path -> Trie.path -> 'hook t

  (** {2 Sequencing} *)

  (** [seq [m0; m1; m2; ...; mn]] runs the modifiers [m0], [m1], [m2], ..., [mn] in order.
      In particular, [seq []] is the identity modifier. *)
  val seq : 'hook t list -> 'hook t

  (** {2 Union} *)

  (** [union [m0; m1; m2; ...; mn]] calculates the union of the results of individual modifiers [m0], [m1], [m2], ..., [mn].
      In particular, [union []] is the empty modifier. *)
  val union : 'hook t list -> 'hook t

  (** {2 Custom Hooks} *)

  (** [hook h] applies the hook labelled [h] to the entire trie.
      See {!module-type:Modifier.S} for the effect [Hook] that will be performed when processing this modifier. *)
  val hook : 'hook -> 'hook t

  (** {2 Ugly Printing} *)

  (** [dump dump_hook m] dumps the internal representation of [m] for debugging,
      where [dump_hook] is the ugly printer for hook labels (see {!val:hook}). *)
  val dump : (Format.formatter -> 'hook -> unit) -> Format.formatter -> 'hook t -> unit
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

    val reperform : (data, tag, hook, context) handler
    (** A handler that reperforms the effects. It can also be used to manually trigger the effects;
        for example, [reperform.not_found (Emp #< "a" #< "b")] will perform the [not_found] effect
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
          let silence_shadow f = run_modifier f {reperform with shadow = fun ?context:_ _ _ y -> y}
        ]}

        Note that {!val:run} runs the code with a fresh empty scope,
        while {!val:run_modifier} remains in the current scope.
    *)

    val reperform : (data, tag, hook, context) handler
    (** A handler that reperforms the internal modifier effects. See {!val:Modifier.S.reperform}. *)

    val modify : ?context:context -> ?prefix:Trie.bwd_path -> hook Language.t -> (data, tag) Trie.t -> (data, tag) Trie.t
    (** Call the internal modifier engine directly on some trie. See {!val:Modifier.S.modify}. *)
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
       | Import of int Trie.Untagged.t * modifier_cmd Language.t
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

     (* Handle scoping effects *)
     let handler : _ Scope.handler =
       let pp_path fmt =
         function
         | Emp -> Format.pp_print_string fmt "(root)"
         | path -> Format.pp_print_string fmt @@ String.concat "." (Bwd.to_list path)
       in
       let pp_context fmt =
         function
         | Some `Visible -> Format.pp_print_string fmt " in the visible namespace"
         | Some `Export -> Format.pp_print_string fmt " in the export namespace"
         | None -> ()
       in
       let pp_item fmt =
         function
         | (x, `Imported) -> Format.fprintf fmt "%i (imported)" x
         | (x, `Local) -> Format.fprintf fmt "%i (local)" x
       in
       { not_found =
           (fun ?context prefix ->
              Format.printf
                "[Warning] Could not find any data within the subtree at %a%a.@."
                pp_path prefix pp_context context);
         shadow =
           (fun ?context path x y ->
              Format.printf
                "[Warning] Data %a assigned at %a was shadowed by data %a%a.@."
                pp_item x
                pp_path path
                pp_item y
                pp_context context;
              y);
         hook =
           (fun ?context prefix hook input ->
              match hook with
              | Print ->
                Format.printf "@[<v 2>[Info] Got the following bindings at %a%a:@;"
                  pp_path prefix pp_context context;
                Trie.iter
                  (fun path x ->
                     Format.printf "%a => %a@;" pp_path path pp_item x)
                  input;
                Format.printf "@]@.";
                input)}

     (* Mute the [shadow] effects. *)
     let silence_shadow f = S.run_modifier f {S.reperform with shadow = fun ?context:_ _ _ y -> y}

     (* The interpreter *)
     let rec interpret_decl : decl -> unit =
       function
       | Decl (p, x) ->
         S.include_singleton ~context_visible:`Visible ~context_export:`Export (p, (x, `Local))
       | ShadowingDecl (p, x) ->
         silence_shadow @@ fun () ->
         S.include_singleton (p, (x, `Local))
       | Import (t, m) ->
         let t = S.modify m (Trie.Untagged.tag `Imported t) in
         S.import_subtree ([], t)
       | PrintVisible ->
         S.modify_visible (Language.hook Print)
       | Export p ->
         S.export_visible (Language.only p)
       | Section (p, sec) ->
         S.section p @@ fun () -> List.iter interpret_decl sec

     let interpret (prog : program) =
       S.run (fun () -> List.iter interpret_decl prog) handler
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
