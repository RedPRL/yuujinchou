(** {1 Types} *)

(** The type of hierarchical names. The name [x.y.z] is represented by the OCaml list [["x"; "y"; "z"]]. *)
type path = string list

(** The abstract type of a trie. *)
type +'a t

(** {1 Basic Operations} *)

(** The empty trie. *)
val empty : 'a t

(** Check whether the trie is empty. *)
val is_empty : 'a t -> bool

(** [root d] makes a trie with the only binding: the root and its associated value [d]. It is equivalent to {!val:root_opt}[(Some d)]. *)
val root : 'a -> 'a t

(** [root_opt d] is equivalent to [match d with None ->]{!val:empty}[| Some d ->]{!val:root}[d]. In other words, [root_opt None] will make an empty trie and [root_opt (Some v)] will make a trie with only one binding: the root associated with the value [v]. If the input is always [Some v], use {!val:root}. *)
val root_opt : 'a option -> 'a t

(** [prefix p t] makes a minimum trie with [t] rooted at [p]. *)
val prefix : path -> 'a t -> 'a t

(** [singleton (p, d)] makes a trie with the only binding: [p] and its associated value [d]. It is equivalent to {!val:prefix}[p @@]{!val:root}[d] *)
val singleton : path * 'a -> 'a t

(** [equal eq t1 t2] checks whether two tries are equal. *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)

(** {1 Finding Values} *)

(** [find_subtree p t] returns the subtree rooted at [p].

    @return The subtrie with all the bindings under [p], including the binding at [p] itself (which will be the root). If there are no such bindings with the prefix [p], an empty trie is returned.
*)
val find_subtree : path -> 'a t -> 'a t

(** [find_singleton p t] returns the value at [p]. *)
val find_singleton : path -> 'a t -> 'a option

(** [find_root t] returns the value at the root. This is equivalent to {!val:find_singleton}[[] t]. *)
val find_root : 'a t -> 'a option

(** {1 Mapping and Filtering} *)

(** [iteri ~rev_prefix f t] applies the function [f] to each value [v] in the trie.

    @param rev_prefix The prefix prepended to any path sent to [f], but in reverse. The default is the empty unit path ([[]]).
*)
val iteri : ?rev_prefix:path -> (rev_path:path -> 'a -> unit) -> 'a t -> unit

(** [mapi ~rev_prefix f t] applies the function [f] to each value [v] in the trie.

    @param rev_prefix The prefix prepended to any path sent to [f], but in reverse. The default is the empty unit path ([[]]).
*)
val mapi : ?rev_prefix:path -> (rev_path:path -> 'a -> 'b) -> 'a t -> 'b t

(** [filteri ~rev_prefix f t] removes all values [v] at path [p] such that [f ~rev_prefix:p v] returns [false].

    @param rev_prefix The prefix prepended to any path sent to [f]. The default is the empty unit path ([[]]).
*)
val filteri : ?rev_prefix:path -> (rev_path:path -> 'a -> bool) -> 'a t -> 'a t

(** [filter_mapi ~rev_prefix f t] applies the function [f] to each value [v] at [p] in the trie. If [f ~rev_prefix:p v] returns [None], then the binding will be removed from the trie. Otherwise, if [f v] returns [Some v'], then the value will be replaced by [v'].

    @param rev_prefix The prefix prepended to any path sent to [f], but in reverse. The default is the empty unit path ([[]]).
*)
val filter_mapi : ?rev_prefix:path -> (rev_path:path -> 'a -> 'b option) -> 'a t -> 'b t

(** {1 Updating} *)

(** [update_subtree p f t] replaces the subtree [t'] rooted at [p] in [t] with [f t']. *)
val update_subtree : path -> ('a t -> 'a t) -> 'a t -> 'a t

(** [update_singleton p f t] replaces the value [v] at [p] in [t] with the result of [f]. If there was no binding at [p], [f None] is evaluated. Otherwise, [f (Some v)] is used. If the result is [None], the old binding at [p] (if any) is removed. Otherwise, if the result is [Some v'], the value at [p] is replaced by [v']. *)
val update_singleton : path -> ('a option -> 'a option) -> 'a t -> 'a t

(** [update_root f t] updates the value at root with [f]. It is equivalent to {!val:update_singleton}[[] f t]. *)
val update_root : ('a option -> 'a option) -> 'a t -> 'a t

(** {1 Union} *)

(** [union ~rev_prefix merger t1 t2] merges two tries [t1] and [t2]. If both tries have a binding at the same path [p], it will call [merger ~rev_path:p x y] to reconcile the values [x] from [t1] and [y] from [t2] that are both bound at the (reversed) path [rev_path]. The path [rev_path] is reversed for efficient traversal.

    @param rev_prefix The prefix prepended to any path sent to [merger]. The default is the empty unit path ([[]]).
*)
val union : ?rev_prefix:path -> (rev_path:path -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

(** [union_subtree ~rev_prefix merger t1 (path, t2)] is equivalent to {!val:union}[~rev_prefix merger t1 (prefix path t2)], but potentially more efficient.

    @param rev_prefix The prefix prepended to any path sent to [merger]. The default is the empty unit path ([[]]).
*)
val union_subtree : ?rev_prefix:path -> (rev_path:path -> 'a -> 'a -> 'a) -> 'a t -> path * 'a t -> 'a t

(** [union_singleton merger t binding] is equivalent to {!val:union}[merger t1 (singleton binding)], but potentially more efficient.

    @param rev_prefix The prefix prepended to any path sent to [merger]. The default is the empty unit path ([[]]).
*)
val union_singleton : ?rev_prefix:path -> (rev_path:path -> 'a -> 'a -> 'a) -> 'a t -> path * 'a -> 'a t

(** {1 Separation} *)

(** [detach_subtree p t] detaches the subtree at [p] from the main trie and returns both the subtree and the remaining trie (in that order). If [detach p t] returns [t1, t2], then {!val:union_subtree}[m t2 (p, t1)] should be equivalent to [t]. *)
val detach_subtree : path -> 'a t -> 'a t * 'a t

(** [detach_singleton p t] detaches the binding at [p] from the main trie and returns both the binding and the remaining trie. If [detach p t] returns [b, t'], then {!val:union_subtree}[m t' (p,]{!val:root_opt}[b)] should be equivalent to [t]. *)
val detach_singleton : path -> 'a t -> 'a option * 'a t

(** {1 Iterators} *)

(** [to_seq ~rev_prefix t] traverses through the trie [t] in the lexicographical order.

    @param rev_prefix The prefix prepended to any path in the output, but in reverse. The default is the empty unit path ([[]]).
*)
val to_seq : ?rev_prefix:path -> 'a t -> (path * 'a) Seq.t

(** [to_seq_with_reversed_paths] is like {!val:to_seq} but with paths reversed. This is potentially more efficient than {!val:to_seq}.

    @param rev_prefix The prefix prepended to any path in the output, but in reverse. The default is the empty unit path ([[]]).
*)
val to_seq_with_reversed_paths : ?rev_prefix:path -> 'a t -> (path * 'a) Seq.t

(** [to_seq_values t] traverses through the trie [t] in the lexicographical order but only returns the associated values. This is potentially more efficient than {!val:to_seq} because path reversal is skipped. *)
val to_seq_values : 'a t -> 'a Seq.t

(** [of_seq ~rev_prefix merger s] inserts bindings [(p, d)] into an empty trie, one by one, using {!val:union_subtree}.

    @param rev_prefix The prefix prepended to any path sent to [merger], but in reverse. The default is the empty unit path ([[]]).
*)
val of_seq : ?rev_prefix:path -> (rev_path:path -> 'a -> 'a -> 'a) -> (path * 'a) Seq.t -> 'a t

module Result :
sig
  (** {1 Updating with results} *)

  (** [update_subtree p f t] replaces the subtree [t'] rooted at [p] in [t] with [f t']. *)
  val update_subtree : path -> ('a t -> ('a t, 'b) result) -> 'a t -> ('a t, 'b) result

  (** [update_singleton p f t] replaces the value [v] at [p] in [t] with the result of [f]. If there was no binding at [p], [f None] is evaluated. Otherwise, [f (Some v)] is used. If the result is [None], the old binding at [p] (if any) is removed. Otherwise, if the result is [Some v'], the value at [p] is replaced by [v']. *)
  val update_singleton : path -> ('a option -> ('a option, 'b) result) -> 'a t -> ('a t, 'b) result

  (** [update_root f t] updates the value at root with [f]. It is equivalent to {!val:update_singleton}[[] f t]. *)
  val update_root : ('a option -> ('a option, 'b) result) -> 'a t -> ('a t, 'b) result
end
