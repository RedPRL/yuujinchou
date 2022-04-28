open Algaeff.StdlibShim

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
module Trie : module type of Trie

(** The {!module:Modifier} module defines the modifiers. *)
module Modifier :
sig
  (** {1 Modifier Type } *)

  (** The type of modifiers, parametrized by the type of hook labels. See {!val:hook}. *)
  type +'hook t

  (**
     The modifier type is abstract---you should build a modifier using the following builders and execute it by {!val:Action.S.run}.
  *)

  (** Checking equality. *)
  val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

  (** {1 Modifier Builders} *)

  (** {2 Basics} *)

  (** [any] keeps the content of the current tree. It is an error if the tree is empty (no name to match).
      To avoid the emptiness checking, use the identity modifier {!val:seq}[[]]. *)
  val any : 'hook t

  (** [only path] keeps the subtree rooted at [path]. It is an error if the subtree was empty. *)
  val only : Trie.path -> 'hook t

  (** [in_ path m] runs the modifier [m] on the subtree rooted at [path]. Bindings outside the subtree are kept intact. For example, [in_ ["x"]]{!val:any} will keep [y] (if existing), while {!val:only}[["x"]] will drop [y]. *)
  val in_ : Trie.path -> 'hook t -> 'hook t

  (** {2 Negation} *)

  (** [none] drops everything. It is an error if the tree was already empty (nothing to drop).
      To avid the emptiness checking, use the empty modifier {!val:union}[[]]. *)
  val none : 'hook t

  (** [except p] drops the subtree rooted at [p]. It is an error if there was nothing in the subtree. This is equivalent to {!val:in_}[p]{!val:none}. *)
  val except : Trie.path -> 'hook t

  (** {2 Renaming} *)

  (** [renaming path path'] relocates the subtree rooted at [path] to [path']. It is an error if the subtree was empty (nothing to move). *)
  val renaming : Trie.path -> Trie.path -> 'hook t

  (** {2 Sequencing} *)

  (** [seq [pat0; pat1; pat2; ...; patn]] runs the modifiers [pat0], [pat1], [pat2], ..., [patn] in order.
      In particular, [seq []] is the identity modifier. *)
  val seq : 'hook t list -> 'hook t

  (** {2 Union} *)

  (** [union [pat0; pat1; pat2; ...; patn]] calculates the union of the results of individual modifiers [pat0], [pat1], [pat2], ..., [patn].
      In particular, [union []] is the empty modifier. *)
  val union : 'hook t list -> 'hook t

  (** {2 Custom Hooks} *)

  (** [hook h] applies the hook labelled [h] to the entire trie. See {!module-type:Action.S} for the effect [Hook] that will be performed when processing this modifier. *)
  val hook : 'hook -> 'hook t

  (** {2 Ugly Printing} *)

  (** [dump dump_hook m] dumps the internal representation of [m] for debugging,
      where [dump_hook] is the ugly printer for hook labels (see {!val:hook}). *)
  val dump : (Format.formatter -> 'hook -> unit) -> Format.formatter -> 'hook t -> unit
end

(** The {!module:Action} module implements the engine running the modifiers. *)
module Action :
sig
  (** The parameters of an engine. *)
  module type Param =
  sig
    (** The type of data held by the bindings. *)
    type data

    (** The type of modifier hook labels. *)
    type hook
  end

  (** The signature of the engine. *)
  module type S =
  sig
    (** @open *)
    include Param

    (** Source of the trie. Each effect may come the source information for the effect handler to identify on which trie the modifier is running. *)
    type source = ..

    (** The effect [BindingNotFound (source, prefix)] means that the engine expected at least one binding within the subtree at [path], but could not find any. Modifiers such as {!val:Modifier.any}, {!val:Modifier.only}, {!val:Modifier.none}, and a few other modifiers expect at least one matching binding. For example, the modifier {!val:Modifier.except}[["x"; "y"]] expects that there was already something under the subtree at [x.y]. If there were actually no names with the prefix [x.y], then the modifier will trigger the effect [BindingNotFound (Emp #< "x" #< "y")]. See {!type:source} for the argument [source]. *)
    type _ Effect.t += BindingNotFound : source option * Trie.bwd_path -> unit Effect.t

    (** The effect [Shadowing (source, path, x, y)] indicates that two items, [x] and [y], are about to be assigned to the same [path]. Modifiers such as {!val:Modifier.renaming} and {!val:Modifier.union} could lead to bindings having the same name, and when that happens, this effect is performed to resolve the conflicting bindings. The effect is continued with the resolution of [x] and [y]. For example, to implement silent shadowing, one can continue it with the item [y]. One can also employ a more sophisticated strategy to implement type-directed disambiguation. See {!type:source} for the argument [source]. *)
    type _ Effect.t += Shadowing : source option * Trie.bwd_path * data * data -> data Effect.t

    (** The effect [Hook (source, path, h, t)] is triggered by modifiers created by {!val:Modifier.hook}. When the engine encounters the modifier {!val:Modifier.hook}[h] while handling the subtree [t] at [path], it will perform the effect [Hook (source, path, h, t)], which may be continued with the resulting trie. See {!type:source} for the argument [source]. *)
    type _ Effect.t += Hook : source option * Trie.bwd_path * hook * data Trie.t -> data Trie.t Effect.t

    (** [run ~prefix modifier trie] runs the [modifier] on the [trie] and return the transformed trie. It can perform effects {!constructor:BindingNotFound}, {!constructor:Shadowing},and {!constructor:Hook}.

        @param source The source of the trie. If unspecified, effects come with {!constructor:None} as their sources.
        @param prefix The prefix prepended to any path or prefix in the effects, but in reverse. The default is the empty unit path ([Emp]).

        @return The new trie after the transformation.
    *)
    val run : ?source:source -> ?prefix:Trie.bwd_path -> hook Modifier.t -> data Trie.t -> data Trie.t
  end

  (** The functor to generate an engine. *)
  module Make (P : Param) : S with type data = P.data and type hook = P.hook
end

(** The {!module:Scope} module implements the scoping effects based on {!module:Action}. *)
module Scope :
sig
  (** The parameters of scoping effects. *)
  module type Param = Action.Param

  (** The signature of scoping effects. *)
  module type S =
  sig
    include Param
    (** @open *)

    exception Locked
    (** The exception [Locked] is raised when an operation on a scope is called
        before another operation on the same scope is finished.
        This could happen when the user calls one of the public functions (for example, {!val:run_on_visible}), and then calls the same
        function or another one (for example, {!val:run_on_export}) while handling the effects performed by the modifier engine
        in {!module:Act}.

        The principle is that you should not access any scope in its intermediate states, including {!val:resolve},
        and any attempt to do so will raise the exception [Locked].

        Note: {!val:section} only locks the parent scope; the child scope is initially unlocked.
    *)

    module Act : Action.S with type data = data and type hook = hook
    (** The modifier engine used by the scoping mechanism to run modifiers.
        Note that {!module:Action.Make} is generative, so it is crucial to handle
        effects defined in this module, not other instances of modifier engines. *)

    type Act.source +=
      | Visible (** The source label for visible namespaces. *)
      | Export (** The source label for export namespaces. *)

    val resolve : Trie.path -> data option
    (** [resolve p] looks up the name [p] in the current scope
        and return the data associated with the binding. *)

    val run_on_visible : hook Modifier.t -> unit
    (** [run_on_visible m] modifies the visible namespace by
        running the modifier [m] on it, using {!val:Act.run}. *)

    val run_on_export : hook Modifier.t -> unit
    (** [run_on_visible m] modifies the export namespace by
        running the modifier [m] on it, using {!val:Act.run}. *)

    val export_visible : hook Modifier.t -> unit
    (** [export_visible m] runs the modifier on the visible namespace,
        using {!val:Act.run}, but then merge the result into the export namespace.
        Conflicting names during the final merge will trigger the effect
        {!constructor:Act.Shadowing}. *)

    val include_singleton : Trie.path * data -> unit
    (** [include_singleton (p, x)] adds a new binding to both the visible
        and export namespaces, where the binding is associating the data [x] to the path [p].
        Conflicting names during the final merge will trigger the effect
        {!constructor:Act.Shadowing}. *)

    val include_subtree : Trie.path * data Trie.t -> unit
    (** [include_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
        both the visible and export namespaces. Conflicting names during the final merge
        will trigger the effect {!constructor:Act.Shadowing}. *)

    val import_subtree : Trie.path * data Trie.t -> unit
    (** [include_subtree (p, ns)] merges the namespace [ns] prefixed with [p] into
        the visible namespace (while keeping the export namespace intact).
        Conflicting names during the final merge will trigger the effect {!constructor:Act.Shadowing}. *)

    val get_export : unit -> data Trie.t
    (** [get_export ()] returns the export namespace of the current scope. *)

    val section : Trie.path -> (unit -> 'a) -> 'a
    (** [section p f] starts a new scope and runs the thunk [f] within the scope.
        The child scope inherits the visible namespace from the parent, and its export namespace
        will be prefixed with [p] and merged into both the visible and export namespaces
        of the parent scope. *)

    val run : ?prefix:Trie.bwd_path -> (unit -> 'a) -> 'a
    (** Execute the code that performs scoping effects.

        @param prefix The additional global prefix prepended to the reported paths originating
        from export namespaces. The default is the empty unit path ([Emp]).
        This does not affect paths originating from visible namespaces. *)
  end

  module Make (P : Param) : S with type data = P.data and type hook = P.hook
  (** The functor to generate a module for scoping effects. *)
end

(**
   {2 Example Code}
*)
(* The example code should be in sync with README.markdown and test/Example.ml *)
(**
   {[
     (* The following shim does nothing for OCaml >= 5, but is needed for OCaml < 5. *)
     open Algaeff.StdlibShim

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
       | Import of int Trie.t * modifier_cmd Modifier.t
       (* printing out all visible bindings *)
       | PrintVisible
       (* exporting a binding *)
       | Export of Trie.path
       (* section *)
       | Section of Trie.path * decl list
     type program = decl list

     (* Specialzed Scope module with Data.t *)
     module S = Scope.Make (struct type data = int type hook = modifier_cmd end)

     (* New source label for imported namespaces *)
     type S.Act.source += Imported

     (* Convert a backward path into a string for printing. *)
     let string_of_bwd_path =
       function
       | Emp -> "(root)"
       | path -> String.concat "." (BwdLabels.to_list path)

     (* Handle effects from running the modifiers. *)
     let handle_modifier_effects f =
       let open Effect.Deep in
       let string_of_source =
         function
         | Some S.Visible -> " in the visible namespace"
         | Some S.Export -> " in the export namespace"
         | Some Imported -> " in the imported namespace"
         | _ -> " in an unknown namespace"
       in
       try_with f ()
         { effc = fun (type a) (eff : a Effect.t) ->
               match eff with
               | S.Act.BindingNotFound (src, path) -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 Format.printf "[Warning] Could not find any data within the subtree at %s%s.@."
                   (string_of_bwd_path path) (string_of_source src);
                 continue k ()
               | S.Act.Shadowing (src, path, old_data, new_data) -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 begin
                   Format.printf "[Warning] Data %i assigned at %s was shadowed by data %i%s.@."
                     old_data (string_of_bwd_path path) new_data (string_of_source src);
                   continue k new_data
                 end
               | S.Act.Hook (src, path, Print, trie) -> Option.some @@
                 fun (k : (a, _) continuation) ->
                 Format.printf "@[<v 2>[Info] Got the following bindings at %s%s:@;"
                   (string_of_bwd_path path) (string_of_source src);
                 Trie.iteri
                   (fun ~path data ->
                      Format.printf "%s => %i@;" (string_of_bwd_path path) data)
                   trie;
                 Format.printf "@]@.";
                 continue k trie
               | _ -> None }

     (* Mute the shadowing effects. *)
     let silence_shadowing f =
       let open Effect.Deep in
       try_with f ()
         { effc = fun (type a) (eff : a Effect.t) ->
               match eff with
               | S.Act.Shadowing (_src, _path, _old_data, new_data) -> Option.some @@
                 fun (k : (a, _) continuation) -> continue k new_data
               | _ -> None }

     (* The interpreter *)
     let rec interpret_decl : decl -> unit =
       function
       | Decl (p, x) ->
         S.include_singleton (p, x)
       | ShadowingDecl (p, x) ->
         silence_shadowing @@ fun () ->
         S.include_singleton (p, x)
       | Import (t, m) ->
         let t = S.Act.run ~source:Imported m t in
         S.import_subtree ([], t)
       | PrintVisible ->
         S.run_on_visible (Modifier.hook Print)
       | Export p ->
         S.export_visible (Modifier.only p)
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
