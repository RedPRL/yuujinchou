open Eff.StdlibShim

(**
   {b Yuujinchou} is an OCaml package of name patterns.

   {1 Introduction}

   This library was motivated by the name modifiers in the "import" or "include" statements present in all practical programming languages.
   Here are a few examples of such statements:

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

   Arguably, common designs of these hiding or renaming mechanisms are quite limited. The goal of the Yuujinchou library is to provide a compositional calculus of these modifiers of names, which we call {i name patterns}. Currently, the library supports renaming, scopes, sequencing, unions, and custom hooks for extending the pattern engine.

   {2 Namespaces and Modules}

   This package intends to treat a namespace as the shared prefix of a group of names; there is no standalone namespace [a], but a group of unrelated declarations that happen to have names sharing the prefix [a]. This design is different from many others that attempt to group a collection of bindings as a standalone module.

   There are fundamental differences between modules (records) and namespaces. A module (or a record) is typed, in the sense that there will be a single type assigned to it. The typedness enables considering an abstract module of a module type. Signatures and functors in Standard ML and OCaml follow this approach. On the other hand, a namespace is untyped, which forbids the notion of abstract namespaces but enables flexible operations such as direct injection of a definition into a namespace. It seems impossible to have a unified design that is typed and supports flexible manipulations. Thus, we (the authors) believe that equating these two will necessarily limit operators on namespaces, which is the case in Standard ML and OCaml. (The modules in Haskell and Agda are namespaces in the above discussion.) Another approach is to have separate notions of namespaces and modules/records, as in C++ and our proof assistant cooltt.
*)

(**
   {1 Using the Library}

   {2 Example Code}
*)
(* The example code should be in sync with README.markdown and test/TestImportSelect.ml *)
(**
   {[
     (* The following shim does nothing for OCaml >= 5, but is needed for OCaml < 5. *)
     open Eff.StdlibShim

     open Yuujinchou
     open Bwd

     module Data =
     struct
       type t = int
       let equal = Int.equal
       let shadow ~path:_ _x y = y
       let compare = Int.compare
     end

     (** An environment is a mapping from paths to data. *)
     type env = Data.t Trie.t

     (* Specialzed Action module with Data.t *)
     type empty = |
     module A = Action.Make (struct type data = Data.t type hook = empty end)

     (** [remap pattern env] uses the [pattern] to massage
         the environment [env]. *)
     let remap pattern env =
       let open Effect.Deep in
       let string_of_path = function [] -> "(root)" | path -> String.concat "." path in
       try_with (A.run pattern) env
         { effc = fun (type a) (eff : a Effect.t) ->
               match eff with
               | A.BindingNotFound path -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 Format.printf "[Warning]@ Could not find any data within the subtree at %s.@."
                   (string_of_path @@ BwdLabels.to_list path);
                 continue k ()
               | A.Shadowing (path, old_data, new_data) -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 if Data.equal old_data new_data then
                   continue k old_data
                 else begin
                   Format.printf "[Warning]@ Data %i assigned at %s was shadowed by data %i.@."
                     old_data (string_of_path @@ BwdLabels.to_list path) new_data;
                   continue k new_data
                 end
               | A.Hook _ -> .
               | _ -> None }

     (** [import env pattern imported] imports the environment
         [imported] massaged by [pattern] into [env]. *)
     let import env pattern imported =
       Trie.union Data.shadow env @@ remap pattern imported

     module DataSet = Set.Make (Data)

     (** [select env pattern] returns the set of matched data. *)
     let select env pattern =
       DataSet.of_seq @@ Trie.to_seq_values @@ remap pattern env
   ]}

   {2 Library Organization}

   The library code is split into four parts: (The {!module:Scope} is new.)
*)

(** The {!module:Trie} module implements mappings from paths to values that support efficient subtree operations. *)
module Trie : module type of Trie

(** The {!module:Pattern} module defines the patterns. *)
module Pattern :
sig
  (** {1 Pattern Type } *)

  (** The type of patterns, parametrized by the type of hook labels. See {!val:hook}. *)
  type +'hook t

  (**
     The pattern type is abstract---you should build a pattern using the following builders and execute it by {!val:Action.S.run}.
  *)

  (** Checking equality. *)
  val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

  (** {1 Pattern Builders} *)

  (** {2 Basics} *)

  (** [any] keeps the content of the current tree. It is an error if the tree is empty (no name to match). *)
  val any : 'hook t

  (** [only path] keeps the subtree rooted at [path]. It is an error if the subtree was empty. *)
  val only : Trie.path -> 'hook t

  (** [in_ path pattern] runs the pattern [pat] on the subtree rooted at [path]. Bindings outside the subtree are kept intact. For example, [in_ ["x"]]{!val:any} will keep [y] (if existing), while {!val:only}[["x"]] will drop [y]. *)
  val in_ : Trie.path -> 'hook t -> 'hook t

  (** {2 Negation} *)

  (** [none] drops everything. It is an error if the tree was already empty (nothing to drop). *)
  val none : 'hook t

  (** [except p] drops the subtree rooted at [p]. It is an error if there was nothing in the subtree. This is equivalent to {!val:in_}[p]{!val:none}. *)
  val except : Trie.path -> 'hook t

  (** {2 Renaming} *)

  (** [renaming path path'] relocates the subtree rooted at [path] to [path']. It is an error if the subtree was empty (nothing to move). *)
  val renaming : Trie.path -> Trie.path -> 'hook t

  (** {2 Sequencing} *)

  (** [seq [pat0; pat1; pat2; ...; patn]] runs the patterns [pat0], [pat1], [pat2], ..., [patn] in order. *)
  val seq : 'hook t list -> 'hook t

  (** {2 Union} *)

  (** [union [pat0; pat1; pat2; ...; patn]] calculates the union of the results of individual patterns [pat0], [pat1], [pat2], ..., [patn]. *)
  val union : 'hook t list -> 'hook t

  (** {2 Custom Hooks} *)

  (** [hook h] applies the hook labelled [h] to the entire trie. See {!module-type:Action.S} for the effect [Hook] that will be performed when processing this pattern. *)
  val hook : 'hook -> 'hook t
end

(** The {!module:Action} module implements the engine running the patterns. *)
module Action :
sig
  (** The parameters of an engine. *)
  module type Param =
  sig
    (** The type of data held by the bindings. *)
    type data

    (** The type of pattern hook labels. *)
    type hook
  end

  (** The signature of the engine. *)
  module type S =
  sig
    (** @open *)
    include Param

    (** The effect [BindingNotFound prefix] means that the engine expected at least one binding under the prefix [prefix], but could not find any. Patterns such as {!val:Pattern.any}, {!val:Pattern.only}, {!val:Pattern.none}, and a few other patterns expect at least one matching binding. For example, the pattern {!val:Pattern.except}[["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the pattern will trigger the effect [BindingNotFound (Emp #< "x" #< "y")]. *)
    type _ Effect.t += BindingNotFound : Trie.bwd_path -> unit Effect.t

    (** The effect [Shadowing (path, x, y)] indicates that two items, [x] and [y], are about to be assigned to the same [path]. Patterns such as {!val:Pattern.renaming} and {!val:Pattern.union} could lead to bindings having the same name, and when that happens, this effect is performed to resolve the conflicting bindings. The effect is continued with the resolution of [x] and [y]. For example, to implement silent shadowing, one can continue it with the item [y]. One can also employ a more sophisticated strategy to implement type-directed disambiguation. *)
    type _ Effect.t += Shadowing : Trie.bwd_path * data * data -> data Effect.t

    (** The effect [Hook (h, prefix, t)] is triggered by patterns created by {!val:Pattern.hook}. When the engine encounters the pattern {!val:Pattern.hook}[h] while handling the trie [t] that is at the prefix [prefix], it will perform the effect [Hook (h, prefix, t)], which may be continued with the resulting trie. *)
    type _ Effect.t += Hook : hook * Trie.bwd_path * data Trie.t -> data Trie.t Effect.t

    (** [run ~prefix pattern trie] runs the [pattern] on the [trie] and return the transformed trie. It can perform effects {!constructor:BindingNotFound}, {!constructor:Shadowing},and {!constructor:Hook}.

        @param prefix The prefix prepended to any path or prefix in the effects, but in reverse. The default is the empty unit path ([[]]).

        @return The new trie after the transformation.
    *)
    val run : ?prefix:Trie.bwd_path -> hook Pattern.t -> data Trie.t -> data Trie.t
  end

  (** The functor to generate an engine. *)
  module Make (P : Param) : S with type data = P.data and type hook = P.hook
end

module Scope :
sig
  (** The parameters of scoping effects. *)
  module type Param = Action.Param

  (** The signature of scoping effects. *)
  module type S =
  sig
    include Param
    (** @open *)

    exception RecursiveLocking
    (** The exception [RecursiveLocking] is raised when an operation on a scope is called
        before another operation on the same scope is finished.
        This could happen when the user calls one of the public functions (for example, {!val:run_on_visible}) and calls the same
        function or another function (for example, {!val:run_on_export}) when handling the effects performed by the pattern engine
        defined in {!module:Act}.
        This module blocks all access to the scope in the intermediate states, including {!val:resolve}, and any attempt
        to access intermediate states of scopes will raise the exception [RecursiveLocking].

        Note: {!val:section} only locks the parent scope; the child scope is initially unlocked.
    *)

    module Act : Action.S with type data = data and type hook = hook
    (** The pattern engine used by the scoping mechanism to run patterns.
        Note that {!module:Action.Make} is generative, so it is crucial to handle
        effects defined in this module, not other instances of pattern engines. *)

    val resolve : Trie.path -> data option
    (** [resolve p] looks up the name [p] in the nested scopes
        and return the data associated with the innermost one.
        (Nested scopes can be formed by calling {!val:section}.) *)

    val run_on_visible : ?prefix:Trie.bwd_path -> hook Pattern.t -> unit
    (** [run_on_visible ?prefix pat] modifies the visible namespace by
        running the pattern pat on it, using {!val:Act.run}.

        @param prefix additional prefix prepended to the paths reported by the effect
        in {!module:Act}. *)

    val run_on_export : ?prefix:Trie.bwd_path -> hook Pattern.t -> unit
    (** [run_on_visible ?prefix pat] modifies the export namespace by
        running the pattern pat on it, using {!val:Act.run}.

        @param prefix additional prefix prepended to the paths reported by the effect
        in {!module:Act}. *)

    val export_visible : ?prefix:Trie.bwd_path -> hook Pattern.t -> unit
    (** [export_visible ?prefix pat] runs the pattern on the visible namespace,
        using {!val:Act.run}, but then merge the result into the export namespace.
        Conflicting names during the final merge will trigger the effect
        {!constructor:Act.Shadowing}.

        @param prefix additional prefix prepended to the paths reported by the effect
        in {!module:Act}. *)

    val include_singleton : ?prefix:Trie.bwd_path -> Trie.path * data -> unit
    (** [include_singleton ?prefix (p, x)] adds a new binding to both the visible
        and export namespaces, where the binding is associating the data [x] to the path [p].
        Conflicting names during the final merge will trigger the effect
        {!constructor:Act.Shadowing}.

        @param prefix additional prefix prepended to the paths reported by the effect
        in {!module:Act}. *)

    val include_subtree : ?prefix:Trie.bwd_path -> Trie.path * data Trie.t -> unit
    (** [include_subtree ?prefix (p, ns)] merges the namespace [ns] prefixed with [p] into
        both the visible and export namespaces. Conflicting names during the final merge
        will trigger the effect {!constructor:Act.Shadowing}.

        @param prefix additional prefix prepended to the paths reported by the effect
        {!constructor:Act.Shadowing}. *)

    val import_subtree : ?prefix:Trie.bwd_path -> Trie.path * data Trie.t -> unit
    (** [include_subtree ?prefix (p, ns)] merges the namespace [ns] prefixed with [p] into
        the visible namespace (while keeping the export namespace intact).
        Conflicting names during the final merge will trigger the effect {!constructor:Act.Shadowing}.

        @param prefix additional prefix prepended to the paths reported by the effect
        {!constructor:Act.Shadowing}. *)

    val run : (unit -> 'a) -> 'a
    (** Execute the code that performs scoping effects. *)

    val section : ?prefix:Trie.bwd_path -> Trie.path -> (unit -> 'a) -> 'a
    (** [section ?prefix p f] starts a new scope and runs the thunk [f] within the scope.
        The child scope inherits the visible namespace from the parent, and its export namespace
        will be prefixed with [p] and merged into both the visible and export namespaces
        of the parent scope.

        @param prefix additional prefix prepended to the paths reported by the effect
        {!constructor:Act.Shadowing}. *)
  end

  module Make (P : Param) : S with type data = P.data and type hook = P.hook
  (** The functor to generate a module for scoping effects. *)
end

(**
   {2 Implementing Features}

   This section shows how mechanisms in other languages can be implemented using this package. We use Haskell and Racket as examples.

   {3 Haskell}

   - Haskell syntax:
   {v
import Mod -- x is available an both x and Mod.x
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(union [any; renaming [] ["Mod"]])
   ]}

   - Haskell syntax:
   {v
import Mod (x,y)
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(union [only ["x"]; only ["y"]])
   ]}

   - Haskell syntax:
   {v
import qualified Mod
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.renaming [] ["Mod"]
   ]}

   - Haskell syntax:
   {v
import qualified Mod hiding (x,y)
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(seq [except ["x"]; except ["y"]; renaming [] ["Mod"]])
   ]}

   {3 Racket}

   - Racket syntax:
   {v
(require (only-in ... id0 [old-id1 new-id1]))
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(seq [...; union [only ["id0"]; seq [only ["old-id1"]; renaming ["old-id1"] ["new-id1"]]]])
   ]}

   - Racket syntax:
   {v
(require (except-in ... id0 id1]))
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(seq [...; except ["id0"]; except ["id1"]])
   ]}

   - Racket syntax:
   {v
(require (prefix-in p: ...))
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(seq [...; renaming [] ["p"]])
   ]}
   Note: Racket does not support hierarchical names, so the prefixing operator in Racket is directly prepending the prefix to the affected names.

   - Racket syntax:
   {v
(require (rename-in ... [old-id0 new-id0] [old-id1 new-id1]))
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(seq [...; renaming ["old-id0"] ["new-id0"]; renaming ["old-id1"] ["new-id1"]])
   ]}

   - Racket syntax:
   {v
(require (combine-in require-spec0 require-spec1 ...))
   v}
   Corresponding Yuujinchou pattern:
   {[
     Pattern.(union [require-spec0; require-spec1; ...])
   ]}

   {1 What is "Yuujinchou"?}

   "Yuujinchou" is the transliteration of "友人帳" in Japanese, which literally means "book of friends". It is a powerful notebook in the manga Natsume Yuujinchou (夏目友人帳) that collects many {e real names (真名)} of youkais (妖怪) (supernatural and spiritual monsters). These real names can be used to summon and control youkais, but the protagonist decided to return the names to their original owners. The plot is about meeting all kinds of youkais.

   This magical book will automatically turn to the page with the correct name when the protagonist pictures the youkai in his mind. This package is also about finding real names of youkais.

   Notes on the transliteration: "Yuujinchou" is in the Wāpuro style so that it uses only the English alphabet; otherwise, its Hepburn romanization would be "Yūjin-chō".
*)
