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

   We can view hiding and renaming as partial functions from names to names. I took this aspect seriously and designed a powerful (possibly overkilling) combinator calculus to express such partial functions---the library you are checking now. It supports renaming, scopes, sequencing, logical connectives, negation and tags with only six combinators in the language. For technical detail, see {!Pattern.core}.
*)

(**
   {1 Organization}

   The code is split into two parts:
*)

(** The {!module:Pattern} module defines the patterns. *)
module Pattern :
sig
  (** {1 Pattern Type } *)

  (** The type of patterns, parametrized by the attribute type. See {!attributes} for more information about attributes. *)
  type 'a pattern

  (**
     The pattern type is abstract---you should build a pattern using the following builders. The detail of the core language is at the end of this page. Use {!val:Action.compile} and {!val:Action.run} to execute a pattern.
  *)

  (** {2 Hierarchical Names} *)

  (** The type of names. *)
  type path = string list

  (**
     We assume names are hierarchical and can be encoded as lists of strings. For example, the name [a.b.c] is represented as the following OCaml list:
     {[
       ["a"; "b"; "c"]
     ]}
  *)

  (** {1 Pattern Builders} *)

  (** {2 Basics} *)

  (** [any] matches any name. *)
  val any : 'a pattern

  (** [only x] matches the name [x] and nothing else. *)
  val only : path -> 'a pattern

  (** [root] matches only the empty name (the empty list [[]]). It is equivalent to [only []]. *)
  val root : 'a pattern

  (** [wildcard] matches everything {e except} the empty name (the empty list [[]]). It is the opposite of [root]. *)
  val wildcard : 'a pattern

  (** [prefix p] matches any name with the given prefix [p] and is equivalent to [scope p any]. *)
  val prefix : path -> 'a pattern

  (** [scope p pat] picks out names with the prefix [p] and runs the pattern [pat] on the remaining part of them.
      For example, [scope ["a"; "b"] pat] on the name [a.b.c.d] will factor out the prefix [a.b]
      and continue running the pattern [pat] on the remaining part [c.d].

      {!val:scope} is more general than {!val:only} and {!val:prefix}: [only x] is equivalent to [scope x root]
      and [prefix p] is equivalent to [scope p any].
  *)
  val scope : path -> 'a pattern -> 'a pattern

  (** {2 Negation} *)

  (** [none] matches no names. It is the opposite of {!val:any}. *)
  val none : 'a pattern

  (** [except x] matches any name {e except} [x]. It is the opposite of {!val:only}. *)
  val except : path -> 'a pattern

  (** [except_prefix p] matches any name that does not have the prefix [p]. It is the opposite of {!val:prefix}. *)
  val except_prefix : path -> 'a pattern

  (** {2 Renaming} *)

  (** [renaming x x'] matches the name [x] and replaces it with [x']. See {!val:only}. *)
  val renaming : path -> path -> 'a pattern

  (** [renaming_prefix p p'] matches any name with the prefix [p] and replaces the prefix with [p']. See {!val:prefix}. *)
  val renaming_prefix : path -> path -> 'a pattern

  (** [renaming_scope p p' pat] is the same as [scope p pat] except that the prefix will be replaced by [p']. See {!val:scope}.

      {!val:renaming_scope} is more general than {!val:renaming} and {!val:renaming_prefix}: [renaming x x'] is equivalent to [renaming_scope x x' root] and [renaming_prefix p p'] is equivalent to [renaming_scope p p' any].
  *)
  val renaming_scope : path -> path -> 'a pattern -> 'a pattern

  (** {2:attributes Attributes} *)

  (**
     Attributes are custom tags attached to matched names. For example, you could attach [`Public] or [`Private] to names when implementing the import statement. You need to supply a lattice structure for your attribute type [t] when compiling a pattern using {!val:Action.compile}, and then specify a default attribute when running the compiled pattern using {!val:Action.run}. Here are the components you need to execute a pattern:

     {ol

     {li

     A join operator of type [t -> t -> t].

     An operator to resolve the conflicting attributes by taking their "union". The exact meaning of "union" depends on the lattice structure. This is for reconciling conflicting attributes when running patterns generated by {!val:seq}, {!val:seq_filter} or {!val:join}.}

     {li
     A meet operator of type [t -> t -> t].

     An operator to resolve the conflicting attributes by taking their "intersection". The exact meaning of "intersection" depends on the lattice structure. This is for reconciling conflicting attributes when running patterns generated by {!val:meet}.
     }

     {li

     A default value of type [t].

     The default attribute attached to the new names until the engine encounters the pattern created via {!attributes} that explicitly sets a new default attribute.
     }}

     For example, when implementing the traditional import statement, the attribute type can be
     {[
       type attr = [ `Public | `Private ]
     ]}
     and then the meet and join operators can be implemented as follows:
     {[
       let join_attr a1 a2 =
         match a1, a2 with
         | `Public, _ | _, `Public -> `Public
         | `Private, `Private -> `Private

       let meet_attr a1 a2 =
         match a1, a2 with
         | `Private, _ | _, `Private -> `Private
         | `Public, `Public -> `Public
     ]}
     In other words, [`Public] is treated as the top element and [`Private] is the bottom element. The rationale is that if a name is simultanously imported as a public name (to be re-exported) and a private name (not to be re-exported), then in most programming languages it {e will} be re-exported. This suggests that the join operator should outputs [`Public] whenever one of the inputs is [`Public]. It then makes sense to make the meet operator the dual of the join operator.

     To pass the lattice structure to the compiler {!val:Action.compile}, use
     {[
       Action.compile ~join:join_attr ~meet:meet pat
     ]}
     To pass the default value to the {!val:Action.run}, use
     {[
       Action.run default path
     ]}
     Please read {!outcomes} on how attributes are attached to the results.

     The following pattern changes the default attribute before running the subpattern:
  *)

  (** [attr a p] assigns the default attribute to [a] and then runs the pattern [p]. *)
  val attr : 'a -> 'a pattern -> 'a pattern

  (** {2 Sequencing} *)

  (** [seq [p0; p1; p2; ...; pn]] runs the patterns [p0], [p1], [p2], ..., [pn] in order.

      If a pattern triggers renaming, then the new names are used in the subsequent patterns. A name is considered matched if it is matched by any pattern during the process. Inconsistent attributes are resolved by the provided [join] operator. See {!attributes}. *)
  val seq : 'a pattern list -> 'a pattern

  (** [seq_filter [p0; p1; p2; ...; pn]] is almost the same as [seq [p0; p1; p2; ...; pn]], except that a name is considered matched only when it is matched (and potentially renamed) by all the patterns in the list. Inconsistent attributes are resolved by the provided [join] operator. See {!attributes}. *)
  val seq_filter : 'a pattern list -> 'a pattern

  (** {2 Logical Connectives} *)

  (** [join [p0; p1; p2; ...; pn]] calculates the "union" of the patterns [p0], [p1], [p2], ..., [pn]. A name is considered matched when it is matched by any subpattern. Inconsistent attributes are resolved by the provided [join] operator on attributes. See {!attributes}. *)
  val join : 'a pattern list -> 'a pattern

  (** [meet [p0; p1; p2; ...; pn]] calculates the "intersection" of the patterns [p0], [p1], [p2], ..., [pn]. There must be at least one subpattern; if the input list is empty, {!val:meet} will raise [Invalid_argument]. A name is considered matched only when it is matched by all the subpatterns. If a name is matched by all subpatterns, but the intersection of the new names is empty, then the name is still considered matched (with an empty set of new names). Inconsistent attributes are resolved by the provided [meet] operator on attributes. See {!attributes}. *)
  val meet : 'a pattern list -> 'a pattern

  (** {2 Unsafe Builders} *)

  (** [unsafe_meet l] is the same as [meet l] except that it does not check whether the list is empty. This might be useful for writing a parser for user-defined patterns. See also {!invariants}. *)
  val unsafe_meet : 'a pattern list -> 'a pattern

  (** [unsafe_inv p] negates the meaning of pattern [p], which might be useful for writing a parser or building more efficient patterns by temporarily violating the invariants. Please consult {!core} for more information on "negation". See also {!invariants}. *)
  val unsafe_inv : 'a pattern -> 'a pattern

  (** {1 Pretty Printers } *)

  (** Pretty printer for {!type:path}. *)
  val pp_path : Format.formatter -> path -> unit

  (** Pretty printer for {!type:pattern}. *)
  val pp_pattern : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a pattern -> unit

  (** {1:core Core Language}

      Here is the current implementation of {!type:pattern}:
      {[
        type 'a pattern =
          | PatWildcard
          | PatScope of path * path option * 'a pattern
          | PatSeq of 'a pattern list
          | PatInv of 'a pattern
          | PatJoin of 'a pattern list
          | PatAttr of 'a * 'a pattern
      ]}

      We will explain each combinator, one by one. However, it is essential to know the outcomes of pattern matching and {e modes} first.

      {2:outcomes Outcomes}

      The result of pattern matching is one of the following:

        {ol
        {li [`NoMatch]: the pattern does not match the name.}
        {li [`Matched [(name_1, attr_1); (name_2, attr_2); ...]]: the pattern matches the name and outputs its new names tagged with attributes. If no renaming happens, then the name list is just a singleton list with the original name. For example, the pattern [Pattern.any] alone keeps the original name and tag it with the default attribute, so running it on a name [a.b] with the default attribute [def] will lead to the output [`Match [["a"; "b"], def]]. The union operator {!val:join} is the major source of multiple new names. It is possible that the set of new names is empty despite the old name being matched because we also support the intersection operator {!val:meet}.}
        }

      See also {!type:Action.matching_result} and {!val:Action.run}.

      {2:modes Modes}

      There are two modes of the pattern matching engine: the {e normal} mode and the {e inverse} mode. The motivation to have the inverse mode is to implement the patterns that hide names from being imported. Think about the pattern [only ["a"; "b"]], which would normally select the name [a.b] from the imported content. Its dual meaning---selecting everything {e other than} the name [a.b]---is exactly the hiding operator we are looking for. The inverse mode has been extended to the entire core language and significantly reduced the number of combinators. It is recommended to study how the core language works under the normal mode first.

      {2 Combinators}

      {3 Wildcard ([PatWildcard])}

      The wildcard pattern [PatWildcard] under the normal mode matches every name except for the root (the empty list [[]]). The same pattern under the inverse mode matches nothing but the root (the empty list [[]]). In either case, if a name [p] is matched, then the output is [`Matched [p, a]] where [a] is the inherited default attribute.

      {3 Scope Renaming ([PatScope])}

      The pattern [PatScope (p, None, pat)] under the normal mode identifies any name with the prefix [p] and then runs [pat] against the residual part of the name. For example, [PatScope (["a"; "b"], None, PatWildcard)] will first identify the name [a.b.c] (represented by [["a"; "b"; "c"]]) because it has the prefix [["a"; "b"]], and then the pattern [PatWildcard] will match the remaining part [c]. The same pattern under the inverse mode will match any name whose prefix is {e not} [p], and any name that has the prefix [p] but fails to match [pat] after removing the prefix [p].

      The pattern [PatScope (p, Some r, pattern)] is the same as [PatScope (p, None, pattern)] expect that the prefix [p], if matched, will be replaced by [r]. The same pattern under the inverse mode will result into an error because the replacement [r] would not be used.

      {3 Sequencing ([PatSeq])}

      The pattern [PatSeq [pat_1; pat_2; ...; pat_n]] under the normal mode runs the patterns [pat_1], [pat_2], ..., [pat_n] in order. New names generated by previous patterns (possibly through the scope renaming operator [PatScope]) are used in later patterns. A name is matched if it is matched by any pattern in the sequence during the process. It is possible that a name is renamed into multiple names during the process, and these renamings are merged by taking the union of all possible renamings, where conflicting attributes are resolved by the join operation in the attribute lattice. As a special case, [PatSeq []] under the normal mode matches nothing.

      The same pattern [PatSeq [pat_1; pat_2; ...; pat_n]] under the inverse mode also runs the patterns [pat_1], [pat_2], ..., [pat_n] in order, but with a twist: a name is matched only if it is matched by all patterns in the sequence. Overlapping renamings and conflicting attributes are resolved by the unions and joins. As a special case, [PatSeq []] under the inverse mode matches any name, including the root (represented by [[]]), which is different from the wildcard pattern ([PatWildcard]).

      {3 Mode Inversion ([PatInv])}

      The pattern [PatInv pat] flips the current mode of the engine (from the normal mode to the inverse mode or vice versa) and proceed with the pattern [pat].

      {3 Join and Meet ([PatJoin])}

      [PatJoin [pat_1; pat_2; ...; pat_n]] under the normal mode runs the subpatterns independently and takes the join of the renaming relations. The same pattern under the inverse mode takes the meet of the renaming relations instead. However, the pattern [PatJoin []] under the inverse mode will result into an error because there is not an unit of the meet of patterns.

      There is one trick case about the meet operation: Assume we have two different names [x] and [y]. The meet of [`Matched [x, a]] and [`Matched [y, a]] is [`Matched []], not [`NoMatch]. It means the name is matched but there are no new names for it.

      {3 Attribute Assignment ([PatAttr])}

      The pattern [PatAttr (attr, pat)] sets the default attribute to [attr] before running the pattern [pat]. It does not change the mode of the engine.


      {2:invariants Invariants}

      Patterns involving renaming (e.g., [PatScope (p, Some r, pattern)]) and the empty join pattern [PatJoin []] should not be run under the inverse mode. This invariant is checked when using {!val:Action.compile} or {!val:Action.compile_} to compile a pattern. It is impossible to violate the invariant unless {!val:unsafe_meet} or {!val:unsafe_inv} is used.
  *)
end

(** The {!module:Action} module implements the engine running the patterns. *)
module Action :
sig
  open Pattern

  (** {1 Types} *)

  (** The abstract type of compiled patterns. *)
  type 'a compiled_pattern

  (** The result type of pattern matching. See {!Pattern.outcomes}. *)
  type 'a matching_result = [
    | `NoMatch (** The pattern does not match the name. *)
    | `Matched of (path * 'a) list (** The pattern matches the name and outputs a list of tagged new names. *)
  ]

  (** The type of errors due to the violation of some invariant of patterns. See {!Pattern.invariants}. It should be impossible to violate these invariants unless {!val:Pattern.unsafe_meet} or {!val:Pattern.unsafe_inv} is used.

      The pattern embedded in the error message is the fragment that violates the invariant. The pattern [pat] in [EmptyMeet pat] is not useful on its own---it must be [PatJoin []]---but it facilitates using or-patterns in error handling. *)
  type 'a error =
    | ReplacementNotUsed of 'a pattern (** Renaming patterns are run under the inverse mode. *)
    | EmptyMeet of 'a pattern (** The join patterns under the inverse mode (or, equivalently, the meet patterns under the normal mode) have no subpatterns. *)

  (** {1 Compilers} *)

  (** The pattern compiler.

      @param join The join operator to resolve conflicting attributes. See {!Pattern.attributes}.
      @param meet The meet operator to resolve conflicting attributes. See {!Pattern.attributes}.
  *)
  val compile : join:('a -> 'a -> 'a) -> meet:('a->'a->'a) -> 'a pattern -> ('a compiled_pattern, 'a error) result

  (** This is {!val:compile} specialized to [unit pattern] where the attribute type is [unit]. *)
  val compile_ : unit pattern -> (unit compiled_pattern, unit error) result

  (** {1 Matching} *)

  (** [run pat ~default path] runs a compiled pattern to match [path] with the default attribute being [default].

      @param default The default attribute for the engine to start with. See {!Pattern.attributes}.
  *)
  val run : 'a compiled_pattern -> default:'a -> path -> 'a matching_result

  (** This is {!val:run} specialized to [unit pattern] where the attribute type is [unit]. *)
  val run_ : unit compiled_pattern -> path -> unit matching_result

  (** {1 Pretty Printers} *)

  (** Pretty printer for {!type:error}. *)
  val pp_error : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a error -> unit

  (** Pretty printer for {!type:matching_result}. *)
  val pp_matching_result : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a matching_result -> unit
end

(**
   {1 How to Use It}

   {[
     open Yuujinchou

     type data = int

     (** An environment is a mapping from paths to data. *)
     type env = (Pattern.path, data) Hashtbl.t

     (** [remap pattern env] uses the [pattern] to massage
         the environment [env]. *)
     let remap pattern env =
       let compiled_pattern = Result.get_ok @@ Action.compile_ pattern in
       let new_env = Hashtbl.create @@ Hashtbl.length env in
       begin
         env |> Hashtbl.iter @@ fun path data ->
         match Action.run_ compiled_pattern path with
         | `NoMatch -> ()
         | `Matched l -> l |> List.iter @@ fun (path, ()) ->
           match Hashtbl.find_opt new_env path with
           | None -> Hashtbl.replace new_env path data
           | Some data' ->
             if data <> data' then
               failwith "Inconsistent data assigned to the same path."
       end;
       new_env

     (** [import env pattern imported] imports the environment
         [imported] massaged by [pattern] into [env]. *)
     let import env pattern imported =
       Hashtbl.replace_seq env @@ Hashtbl.to_seq @@ remap pattern imported
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
     join [any; renaming_prefix [] ["Mod"]]
   ]}

   {v
import Mod (x,y)
   v}
   {[
     join [only ["x"]; only ["y"]]
   ]}

   {v
import qualified Mod
   v}
   {[
     join [renaming_prefix [] ["Mod"]]
   ]}

   {v
import qualified Mod hiding (x,y)
   v}
   {[
     renaming_scope [] ["Mod"] @@ meet [hide ["x"]; hide ["y"]]
   ]}

   {2 Racket}

   {v
(require (only-in ... id0 [old-id1 new-id1]))
   v}
   {[
     seq_filter [...; join [only ["id0"]; renaming ["old-id1"] ["new-id1"]]]
   ]}

   {v
(require (except-in ... id0 id1]))
   v}
   {[
     seq_filter [...; except ["id0"]; except ["id1"]]
   ]}

   {v
(require (prefix-in p: ...))
   v}
   {[
     seq [...; renaming_prefix [] ["p:"]]
   ]}

   {v
(require (rename-in ... [old-id0 new-id0] [old-id1 new-id1]))
   v}
   {[
     seq [...; join [renaming ["old-id0"] ["new-id0"]; renaming ["old-id1"] ["new-id1"]]]
   ]}

   {v
(require (combine-in require-spec0 require-spec1 ...))
   v}
   {[
     join [require-spec0; require-spec1; ...]
   ]}

   The [provide] mechanism can be simulated in a similar way. This library does not directly support the phase levels in Racket (yet).

   {1 What is "Yuujinchou"?}

   "Yuujinchou" is the transliteration of "友人帳" in Japanese, which literally means "book of friends". It is a powerful notebook in the manga Natsume Yuujinchou (夏目友人帳) that collects many {e real names (真名)} of youkais (妖怪) (supernatural and spiritual monsters). These real names can be used to summon and control youkais, but the protagonist decided to return the names to their original owners. The plot is about meeting all kinds of youkais.

   This magical book will automatically turn to the page with the correct name when the protagonist pictures the youkai in his mind. This library is also about finding real names of youkais.

   The transliteration is in the Wāpuro style to use only English alphabet letters; otherwise, its Hepburn romanization would be "Yūjin-chō".

*)
