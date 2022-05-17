open Bwd

(** {1 Types} *)

(** The type of hierarchical names. The name [x.y.z] is represented by the OCaml list [["x"; "y"; "z"]]. *)
type path = string list

(** The type of hierarchical names, but using backward lists. The name [x.y.z] is represented by the backward list [Emp #< "x" #< "y" #< "z"]. *)
type bwd_path = string bwd

(** The abstract type of a trie. *)
type (+!'data, +!'tag) t

(** {1 Basic Operations} *)

(** The empty trie. *)
val empty : ('data, 'tag) t

(** Check whether the trie is empty. *)
val is_empty : ('data, 'tag) t -> bool

(** [root (d, t)] makes a trie with the only binding: the root and its associated value [d] and tag [t]. It is equivalent to {!val:root_opt}[ (Some (d, t))]. *)
val root : 'data * 'tag -> ('data, 'tag) t

(** [root_opt v] is equivalent to [match v with None -> ]{!val:empty}[ | Some v -> ]{!val:rot}[ v]. In other words, [root_opt None] will make an empty trie and [root_opt (Some (d, t))] will make a trie with only one binding: the root associated with the value [d] and the tag [t]. If the input is always [Some v], use {!val:root}. *)
val root_opt : ('data * 'tag) option -> ('data, 'tag) t

(** [prefix p t] makes a minimum trie with [t] rooted at [p]. *)
val prefix : path -> ('data, 'tag) t -> ('data, 'tag) t

(** [singleton (p, d)] makes a trie with the only binding: [p] and its associated value [d]. It is equivalent to {!val:prefix}[ p @@ ]{!val:root}[ d] *)
val singleton : path * ('data * 'tag) -> ('data, 'tag) t

(** [equal eq_data eq_tag t1 t2] checks whether two tries are equal. *)
val equal : ('data -> 'data -> bool) -> ('tag -> 'tag -> bool) -> ('data, 'tag) t -> ('data, 'tag) t -> bool

(** {1 Finding Values} *)

(** [find_subtree p t] returns the subtree rooted at [p].

    @return The subtrie with all the bindings under [p], including the binding at [p] itself (which will be the root). If there are no such bindings with the prefix [p], an empty trie is returned.
*)
val find_subtree : path -> ('data, 'tag) t -> ('data, 'tag) t

(** [find_singleton p t] returns the value at [p]. *)
val find_singleton : path -> ('data, 'tag) t -> ('data * 'tag) option

(** [find_root t] returns the value at the root. This is equivalent to {!val:find_singleton}[ [] t]. *)
val find_root : ('data, 'tag) t -> ('data * 'tag) option

(** {1 Mapping and Filtering} *)

(** [iter ~prefix f t] applies the function [f] to each value [v] in the trie.

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val iter : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> unit) -> ('data, 'tag) t -> unit

(** [map_data ~prefix f t] applies the function [f] to each value [v] in the trie.

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val map_data : ?prefix:bwd_path -> (bwd_path -> 'data1 * 'tag -> 'data2) -> ('data1, 'tag) t -> ('data2, 'tag) t

(** [filter ~prefix f t] removes all values [v] at path [p] such that [f ~prefix:p v] returns [false].

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val filter : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> bool) -> ('data, 'tag) t -> ('data, 'tag) t

(** [filter_map_data ~prefix f t] applies the function [f] to each value [v] at [p] in the trie. If [f ~prefix:p v] returns [None], then the binding will be removed from the trie. Otherwise, if [f v] returns [Some v'], then the value will be replaced by [v'].

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val filter_map_data : ?prefix:bwd_path -> (bwd_path -> 'data1 * 'tag -> 'data2 option) -> ('data1, 'tag) t -> ('data2, 'tag) t

(** {1 Updating} *)

(** [update_subtree p f t] replaces the subtree [t'] rooted at [p] in [t] with [f t']. *)
val update_subtree : path -> (('data, 'tag) t -> ('data, 'tag) t) -> ('data, 'tag) t -> ('data, 'tag) t

(** [update_singleton p f t] replaces the value [v] at [p] in [t] with the result of [f]. If there was no binding at [p], [f None] is evaluated. Otherwise, [f (Some v)] is used. If the result is [None], the old binding at [p] (if any) is removed. Otherwise, if the result is [Some v'], the value at [p] is replaced by [v']. *)
val update_singleton : path -> (('data * 'tag) option -> ('data * 'tag) option) -> ('data, 'tag) t -> ('data, 'tag) t

(** [update_root f t] updates the value at root with [f]. It is equivalent to {!val:update_singleton}[ [] f t]. *)
val update_root : (('data * 'tag) option -> ('data * 'tag) option) -> ('data, 'tag) t -> ('data, 'tag) t

(** {1 Union} *)

(** [union ~prefix merger t1 t2] merges two tries [t1] and [t2]. If both tries have a binding at the same path [p], it will call [merger p x y] to reconcile the values [x] from [t1] and [y] from [t2] that are both bound at the [path].

    @param prefix The prefix prepended to any path sent to [merger]. The default is the empty prefix ([Emp]).
*)
val union : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag) -> ('data, 'tag) t -> ('data, 'tag) t -> ('data, 'tag) t

(** [union_subtree ~prefix merger t1 (path, t2)] is equivalent to {!val:union}[ ~prefix merger t1 (prefix path t2)], but potentially more efficient.

    @param prefix The prefix prepended to any path sent to [merger]. The default is the empty prefix ([Emp]).
*)
val union_subtree : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag) -> ('data, 'tag) t -> path * ('data, 'tag) t -> ('data, 'tag) t

(** [union_singleton merger t binding] is equivalent to {!val:union}[ merger t1 (singleton binding)], but potentially more efficient.

    @param prefix The prefix prepended to any path sent to [merger]. The default is the empty prefix ([Emp]).
*)
val union_singleton : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag) -> ('data, 'tag) t -> path * ('data * 'tag) -> ('data, 'tag) t

(** [union_root merger t r] is equivalent to {!val:union_singleton}[ merger t ([], r)], but potentially more efficient.

    @param prefix The prefix prepended to any path sent to [merger]. The default is the empty prefix ([Emp]).
*)
val union_root : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag) -> ('data, 'tag) t -> 'data * 'tag -> ('data, 'tag) t

(** {1 Separation} *)

(** [detach_subtree p t] detaches the subtree at [p] from the main trie and returns both the subtree and the remaining trie (in that order). If [detach p t] returns [t1, t2], then {!val:union_subtree}[ m t2 (p, t1)] should be equivalent to [t]. *)
val detach_subtree : path -> ('data, 'tag) t -> ('data, 'tag) t * ('data, 'tag) t

(** [detach_singleton p t] detaches the binding at [p] from the main trie and returns both the binding and the remaining trie. If [detach p t] returns [b, t'], then {!val:union_subtree}[ m t' (p, ]{!val:root_opt}[ b)] should be equivalent to [t]. *)
val detach_singleton : path -> ('data, 'tag) t -> ('data * 'tag) option * ('data, 'tag) t

(** [detach_root t] detaches the binding at the root of and returns both the binding and the remaining trie. It is equivalent to {!val:detach_singleton}[ [] t]. *)
val detach_root : ('data, 'tag) t -> ('data * 'tag) option * ('data, 'tag) t

(** {1 Iterators} *)

(** [to_seq ~prefix t] traverses through the trie [t] in the lexicographical order.

    @param prefix The prefix prepended to any path in the output. The default is the empty prefix ([Emp]).
*)
val to_seq : ?prefix:bwd_path -> ('data, 'tag) t -> (path * ('data * 'tag)) Seq.t

(** [to_seq_with_bwd_paths] is like {!val:to_seq}. This is potentially more efficient than {!val:to_seq} because the conversion from backward lists to forward lists is skipped.

    @param prefix The prefix prepended to any path in the output. The default is the empty prefix ([Emp]).
*)
val to_seq_with_bwd_paths : ?prefix:bwd_path -> ('data, 'tag) t -> (bwd_path * ('data * 'tag)) Seq.t

(** [to_seq_values t] traverses through the trie [t] in the lexicographical order but only returns the associated values. This is potentially more efficient than {!val:to_seq} because the conversion from backward lists to forward lists is skipped. *)
val to_seq_values : ('data, 'tag) t -> 'data Seq.t

(** [to_seq_values_with_tags t] traverses through the trie [t] in the lexicographical order but only returns the associated values and tags. This is potentially more efficient than {!val:to_seq} because the conversion from backward lists to forward lists is skipped. *)
val to_seq_values_with_tags : ('data, 'tag) t -> ('data * 'tag) Seq.t

(** [of_seq ~prefix s] inserts bindings [(p, d)] into an empty trie, one by one, using {!val:union_singleton}. Later bindings will shadow previous ones if the paths of bindings are not unique. *)
val of_seq : (path * ('data * 'tag)) Seq.t -> ('data, 'tag) t

(** [of_seq_with_merger ~prefix merger s] inserts bindings [(p, d)] into an empty trie, one by one, using {!val:union_singleton}. Bindings with the same path are resolved using [merger] instead of silent shadowing.

    @param prefix The prefix prepended to any path sent to [merger]. The default is the empty prefix ([Emp]). Note that [prefix] does not directly affect the output trie, only the argument to [merger].
*)
val of_seq_with_merger : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag) -> (path * ('data * 'tag)) Seq.t -> ('data, 'tag) t

(** {1 Tags} *)

(** The abstract type of an untagged trie. *)
type +!'data untagged

(** Attach a tag to all existing bindings in O(1) time. *)
val tag : 'tag -> 'data untagged -> ('data, 'tag) t

(** [untag t] strips off tags from tries in O(1) time. *)
val untag : ('data, _) t -> 'data untagged

(** [retag tag t] changes all tags within [t] to [tag] in O(1) time. *)
val retag : 'tag -> ('data, _) t -> ('data, 'tag) t

module Untagged :
sig
  type 'data t = 'data untagged

  (** [to_seq ~prefix t] traverses through the trie [t] in the lexicographical order.

      @param prefix The prefix prepended to any path in the output. The default is the empty prefix ([Emp]).
  *)
  val to_seq : ?prefix:bwd_path -> 'data t -> (path * 'data) Seq.t

  (** [to_seq_values t] traverses through the trie [t] in the lexicographical order but only returns the associated values. This is potentially more efficient than {!val:to_seq} because the conversion from backward lists to forward lists is skipped. *)
  val to_seq_values : 'data t -> 'data Seq.t

  (** [to_seq_with_bwd_paths] is like {!val:to_seq}. This is potentially more efficient than {!val:to_seq} because the conversion from backward lists to forward lists is skipped.

      @param prefix The prefix prepended to any path in the output. The default is the empty prefix ([Emp]).
  *)
  val to_seq_with_bwd_paths : ?prefix:bwd_path -> 'data t -> (bwd_path * 'data) Seq.t

  (** [of_seq ~prefix s] inserts bindings [(p, d)] into an empty trie, one by one, using {!val:union_singleton}. Later bindings will shadow previous ones if the paths of bindings are not unique. *)
  val of_seq : (path * 'data) Seq.t -> 'data t
end
