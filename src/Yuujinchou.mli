(**
   {b Yuujinchou} is an OCaml package of name patterns. It was motivated by the "import" or "include" statements present in almost all programming languages. Here are a few examples:

   {v open import M -- Agda v}

   {v import foo # Python v}

   The ability to import content from other files helps organize code. However, it also poses a new challenge: how could programmers prevent imported content from shadowing existing content? For example, if we already have a function [test] in the current scope, maybe we do not wish to import another function also named [test]. To address this, many programming languages allow programmers to selectively hide or rename part of the imported content:

   {v
open import M renaming (a to b) public
-- renaming a to b, and then re-exporting the content
   v}

   {v
import foo as bar
# putting content of foo under the prefix bar
   v}

   We can view a collection of hierarchical names as a tree, and see these modifiers as tree transformers. I took this aspect seriously and designed a powerful (possibly overkilling) combinator calculus to express such tree transformers---the library you are checking now. It supports renaming, scopes, sequencing, unions, generic filtering.
*)

(**
   {1 Organization}

   The code is split into three parts:
*)

(** The {!module:Trie} module implements a data structure (tries) that supports efficient subtree operators on a collection of hierarchical names and their associated data. Currently, only the minimum API to implement the pattern engine is exposed. *)
module Trie : module type of (Trie)

(** The {!module:Pattern} module defines the patterns. *)
module Pattern :
sig
  (** {1 Pattern Type } *)

  (** The type of patterns, parametrized by the type of associated data. *)
  type 'a t

  (**
     The pattern type is abstract---you should build a pattern using the following builders and execute it by {!val:Action.run}.
  *)

  (** Checking equality. Note that patterns created by {!val:filter_map} are not comparable to each other because they take a function. *)
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  (** {2 Hierarchical Names} *)

  (** The type of names. *)
  type path = string list

  (**
     We assume names are hierarchical and can be encoded as lists of strings. For example, the name [x.y.z] is represented as the following OCaml list:
     {[
       ["x"; "y"; "z"]
     ]}
  *)

  (** {1 Pattern Builders} *)

  (** {2 Basics} *)

  (** [any] keeps the content of the current tree. It is an error if the tree is empty (no name to match). *)
  val any : 'a t

  (** [root] keeps only the empty name (the empty list [[]]). It is equivalent to [only []]. *)
  val root : 'a t

  (** [wildcard] keeps everything {e except} the empty name (the empty list [[]]). It is an error if there was no name is matched. *)
  val wildcard : 'a t

  (** [only x] keeps the name [x] and drops everything else. It is an error if there was no binding named [x] in the tree. *)
  val only : path -> 'a t

  (** [only_subtree path] keeps the subtree rooted at [path]. It is an error if the subtree was empty. *)
  val only_subtree : path -> 'a t

  (** [on_subtree path pattern] runs the pattern [pat] against the subtree rooted at [path]. Names that were not in the subtree are kept. *)
  val in_subtree : path -> 'a t -> 'a t

  (** {2 Negation} *)

  (** [none] drops everything. It is an error if the tree was already empty (nothing to drop). *)
  val none : 'a t

  (** [except x] drops the binding named [x]. It is an error if there was no [x] from the beginning. *)
  val except : path -> 'a t

  (** [except_subtree p] drops the subtree rooted at [p]. It is an error if there was nothing in the subtree. This is equivalent to [in_subtree p none]. *)
  val except_subtree : path -> 'a t

  (** {2 Renaming} *)

  (** [renaming x x'] renames the name [x] into [x']. It is an error if there was no [x] from the beginning. *)
  val renaming : path -> path -> 'a t

  (** [renaming_subtree path path'] relocates the subtree rooted at [path] to [path']. It is an error if the subtree was empty. *)
  val renaming_subtree : path -> path -> 'a t

  (** {2 Associated Data} *)

  (** [filter_map f] applies the function [f] to the associated data in the tree. If the result is [None], the binding would be removed from the tree. Otherwise, if the result is [Some x], the content of the binding would be replaced with [x]. *)
  val filter_map : ('a -> 'a option) -> 'a t

  (** {2 Sequencing} *)

  (** [seq [pat0; pat1; pat2; ...; patn]] runs the patterns [pat0], [pat1], [pat2], ..., [patn] in order. *)
  val seq : 'a t list -> 'a t

  (** {2 Union} *)

  (** [union [pat0; pat1; pat2; ...; patn]] calculates the union of the results of individual patterns [pat0], [pat1], [pat2], ..., [patn]. *)
  val union : 'a t list -> 'a t

  (** {1 Pretty Printers } *)

  (** Pretty printer for {!type:t}. *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

(** The {!module:Action} module implements the engine running the patterns. *)
module Action :
sig
  (** {1 Error Type} *)

  (** The type of errors due to the absense of expected bindings. For example, the pattern [Pattern.except_subtree ["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were no names with the prefix [x.y], then the pattern will result into the error [BindingNotFound ["x"; "y"]]. *)
  type error = BindingNotFound of Pattern.path (** The engine could not find the expected bindings. *)

  (** {1 Matching} *)

  (** [run merger pattern trie] runs the [pattern] on the [trie]

      @param merger The resolver for two conflicting bindings sharing the same name. Patterns such as [Pattern.renaming] and [Pattern.union] could lead to conflicting bindings, and [merger x y] should return the resolution of [x] and [y].
  *)
  val run : ('a -> 'a -> 'a) -> 'a Pattern.t -> 'a Trie.t -> ('a Trie.t, error) result

  (** {1 Pretty Printers} *)

  (** Pretty printer for {!type:error}. *)
  val pp_error : Format.formatter -> error -> unit
end

(**
   {1 How to Use It}

   {[
     open Yuujinchou

     module Data =
     struct
       type t = int
       let equal n1 n2 = n1 = n2
       let merge x y =
         if equal x y then x
         else failwith "Inconsistent data assigned to the same path."
       let shadow _x y = y
       let compare : t -> t -> int = compare
     end

     (** An environment is a mapping from paths to data. *)
     type env = Data.t Trie.t

     (** [remap pattern env] uses the [pattern] to massage
         the environment [env]. *)
     let remap pattern env =
       match Action.run Data.merge pattern env with
       | Ok env -> env
       | Error (Action.BindingNotFound path) ->
         failwith ("Expected item not found within the subtree rooted at #root." ^ String.concat "." path ^ ".")

     (** [import env pattern imported] imports the environment
         [imported] massaged by [pattern] into [env]. *)
     let import env pattern imported =
       Trie.union Data.shadow env @@ remap pattern imported

     module DataSet = Set.Make (Data)

     (** [select env pattern] returns the set of matched data. *)
     let select env pattern =
       DataSet.of_seq @@ Trie.to_seq_values @@ remap pattern env
   ]}
*)

(**
   {1  Namespace?}

   This library intends to treat a namespace as the prefix of a group of names. That is, there is no namespace [a], but only a group of unrelated names that happen to have the prefix [a].

   Note that namespaces (name prefixes of unrelated items) are different from modules (groups of items that are bound together). This library does not provide special support for modules (yet).

   {1 Examples from Other Languages}

   {2 Haskell}

   {v
import Mod -- x is available an both x and Mod.x
   v}
   {[
     union [any; renaming_subtree [] ["Mod"]]
   ]}

   {v
import Mod (x,y)
   v}
   {[
     union [only ["x"]; only ["y"]]
   ]}

   {v
import qualified Mod
   v}
   {[
     renaming_subtree [] ["Mod"]
   ]}

   {v
import qualified Mod hiding (x,y)
   v}
   {[
     seq [except ["x"]; except ["y"]; renaming_subtree [] ["Mod"]]
   ]}

   {2 Racket}

   {v
(require (only-in ... id0 [old-id1 new-id1]))
   v}
   {[
     seq [...; union [only ["id0"]; seq [only ["old-id1"]; renaming ["old-id1"] ["new-id1"]]]]
   ]}

   {v
(require (except-in ... id0 id1]))
   v}
   {[
     seq [...; except ["id0"]; except ["id1"]]
   ]}

   {v
(require (prefix-in p: ...))
   v}
   {[
     seq [...; renaming_subtree [] ["p"]]
   ]}

   {v
(require (rename-in ... [old-id0 new-id0] [old-id1 new-id1]))
   v}
   {[
     seq [...; renaming ["old-id0"] ["new-id0"]; renaming ["old-id1"] ["new-id1"]]
   ]}

   {v
(require (combine-in require-spec0 require-spec1 ...))
   v}
   {[
     union [require-spec0; require-spec1; ...]
   ]}

   The [provide] mechanism can be simulated in a similar way, and the phases can be done by {!val:Pattern.filter_map}.

   {1 What is "Yuujinchou"?}

   "Yuujinchou" is the transliteration of "友人帳" in Japanese, which literally means "book of friends". It is a powerful notebook in the manga Natsume Yuujinchou (夏目友人帳) that collects many {e real names (真名)} of youkais (妖怪) (supernatural and spiritual monsters). These real names can be used to summon and control youkais, but the protagonist decided to return the names to their original owners. The plot is about meeting all kinds of youkais.

   This magical book will automatically turn to the page with the correct name when the protagonist pictures the youkai in his mind. This library is also about finding real names of youkais.

   The transliteration is in the Wāpuro style to use only English alphabet letters; otherwise, its Hepburn romanization would be "Yūjin-chō".

*)
