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

   {[
     open Yuujinchou

     module Data =
     struct
       type t = int
       let equal n1 n2 = n1 = n2
       let merge ~rev_path x y =
         if equal x y then
           Result.ok x
         else
           Result.error @@ `Inconsistent (List.rev rev_path)
       let shadow ~rev_path:_ _x y = y
       let compare : t -> t -> int = compare
     end

     (** An environment is a mapping from paths to data. *)
     type env = Data.t Trie.t

     (** [remap pattern env] uses the [pattern] to massage
         the environment [env]. *)
     let remap pattern env =
       let pp_path = function [] -> "(root)" | path -> String.concat "." path in
       match Action.run ~union:Data.merge pattern env with
       | Ok env -> env
       | Error (`Inconsistent path) ->
         failwith ("Inconsistent data assigned to the same path " ^ pp_path path)
       | Error (`BindingNotFound path) ->
         failwith ("Expected binding(s) not found within the subtree at " ^ pp_path path)

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

   The library code is split into three parts:
*)

(** The {!module:Trie} module implements mappings from paths to values that support efficient subtree operations. *)
module Trie : module type of Trie

(** The {!module:Pattern} module defines the patterns. *)
module Pattern :
sig
  (** {1 Pattern Type } *)

  (** The type of hierarchical names. *)
  type path = string list

  (**
     We assume names are hierarchical and can be encoded as lists of strings. For example, the name [x.y.z] is represented as the following OCaml list:
     {[
       ["x"; "y"; "z"]
     ]}
  *)

  (** The type of patterns, parametrized by the type of hook labels. See {!val:hook}. *)
  type +'hook t

  (**
     The pattern type is abstract---you should build a pattern using the following builders and execute it by {!val:Action.run}.
  *)

  (** Checking equality. *)
  val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

  (** {1 Pattern Builders} *)

  (** {2 Basics} *)

  (** [any] keeps the content of the current tree. It is an error if the tree is empty (no name to match). *)
  val any : 'hook t

  (** [only path] keeps the subtree rooted at [path]. It is an error if the subtree was empty. *)
  val only : path -> 'hook t

  (** [in_ path pattern] runs the pattern [pat] on the subtree rooted at [path]. Bindings outside the subtree are kept intact. For example, [in_ ["x"]]{!val:any} will keep [y] (if existing), while {!val:only}[["x"]] will drop [y]. *)
  val in_ : path -> 'hook t -> 'hook t

  (** {2 Negation} *)

  (** [none] drops everything. It is an error if the tree was already empty (nothing to drop). *)
  val none : 'hook t

  (** [except p] drops the subtree rooted at [p]. It is an error if there was nothing in the subtree. This is equivalent to {!val:in_}[p]{!val:none}. *)
  val except : path -> 'hook t

  (** {2 Renaming} *)

  (** [renaming path path'] relocates the subtree rooted at [path] to [path']. It is an error if the subtree was empty (nothing to move). *)
  val renaming : path -> path -> 'hook t

  (** {2 Sequencing} *)

  (** [seq [pat0; pat1; pat2; ...; patn]] runs the patterns [pat0], [pat1], [pat2], ..., [patn] in order. *)
  val seq : 'hook t list -> 'hook t

  (** {2 Union} *)

  (** [union [pat0; pat1; pat2; ...; patn]] calculates the union of the results of individual patterns [pat0], [pat1], [pat2], ..., [patn]. *)
  val union : 'hook t list -> 'hook t

  (** {2 Custom Hooks} *)

  (** [hook h] applies the hook labelled [h] to the entire trie; see {!val:Action.run_with_hooks}. *)
  val hook : 'hook -> 'hook t

  (** {1 Utility Functions } *)

  (** Pretty printer for {!type:path}. *)
  val pp_path : Format.formatter -> Pattern.path -> unit
end

(** The {!module:Action} module implements the engine running the patterns. *)
module Action :
sig
  (** The engine tries to preserve physical equality whenever feasible. For example, if the trie [t] has a binding at [x], the pattern {!val:Pattern.renaming}[["x"] ["x"]] on [t] will return the original [t]. *)

  (** {1 Matching} *)

  (** The type of the result. The effect [`BindingNotFound] means that the engine expected at least one binding under [path] but could not find it. *)
  type _ Effect.t += BindingNotFound : Pattern.path -> unit Effect.t

  (** [run ~rev_prefix ~union pattern trie] runs the [pattern] on the [trie] and return the transformed trie. It ignores patterns created by {!val:Pattern.hook}.

      @param rev_prefix The prefix prepended to any path sent to [union] and any path in the error reporting, but in reverse. The default is the empty unit path ([[]]).
      @param union The resolver for two conflicting bindings sharing the same name. Patterns such as {!val:Pattern.renaming} and {!val:Pattern.union} could lead to conflicting bindings, and [union ~rev_path x y] should return the resolution of [x] and [y] at the (reversed) path [rev_path].

      @return The new trie after the transformation. [Error (`BindingNotFound p)] means the transformation failed because of the absence of expected bindings. For example, the pattern {!val:Pattern.except}[["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the pattern will trigger the error [`BindingNotFound ["x"; "y"]]. The path [p] is only an approximation---the user might have intended to hide the binding at [["x"; "y"; "z"]], a binding under [["x"; "y"]], but the engine would never know the user's true intention. *)
  val run :
    ?rev_prefix:Pattern.path ->
    union:(rev_path:Pattern.path -> 'a -> 'a -> 'a) ->
    unit Pattern.t -> 'a Trie.t -> 'a Trie.t

  (** [run_with_hooks ~rev_prefix ~hooks ~union pattern trie] runs the [pattern] on the [trie] and return the transformed trie. It is similar to {!val:run} but accepts an additional argument [hooks] to handle the patterns created by {!val:Pattern.hook}.

      @param rev_prefix Same as the parameter [rev_prefix] for {!val:run}.
      @param union Same as the parameter [union] for {!val:run}.
      @param hooks The hooks that will be triggered by patterns created by {!val:Pattern.hook}. When the engine encounters the pattern {!val:Pattern.hook}[h], it will call [hooks h ~rev_path:p t] on the current trie where [p] is the path (in reverse) and [t] is the subtrie at [p].
  *)
  val run_with_hooks :
    ?rev_prefix:Pattern.path ->
    union:(rev_path:Pattern.path -> 'a -> 'a -> 'a) ->
    hooks:('hook -> rev_prefix:Pattern.path -> 'a Trie.t -> 'a Trie.t) ->
    'hook Pattern.t -> 'a Trie.t -> 'a Trie.t

  (** {1 Pretty Printers} *)

  (** Pretty printer for {!type:path}. *)
  val pp_path : Format.formatter -> Pattern.path -> unit
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
