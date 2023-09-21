open Bwd

(** {1 Types} *)

(** The type of hierarchical names. The name [x.y.z] is represented by the OCaml list [["x"; "y"; "z"]]. *)
type path = string list

(** The type of hierarchical names, but using backward lists. The name [x.y.z] is represented by the backward list [Emp #< "x" #< "y" #< "z"]. *)
type bwd_path = string bwd

(** The abstract type of a trie.
    ['data] represents the information surviving retagging, and ['tag] represents the information to be reset during retagging.
    See {!val:retag}, which could reset all tags in O(1) time while keeping data intact.
    A possible usage when making a proof assistant is to put top-level definitions into ['data]
    and identities of the import statements into ['tag] for efficient detection of unused imports. *)
type (+!'data, +!'tag) t

(** {1 Basic Operations} *)

(** The empty trie. *)
val empty : ('data, 'tag) t

(** Check whether the trie is empty. *)
val is_empty : ('data, 'tag) t -> bool

(** [root (d, t)] makes a trie with the only one binding: the root and its associated data [d] and tag [t]. It is equivalent to {!val:root_opt}[ (Some (d, t))]. *)
val root : 'data * 'tag -> ('data, 'tag) t

(** [root_opt v] is equivalent to [match v with None -> ]{!val:empty}[ | Some v -> ]{!val:root}[ v]. In other words, [root_opt None] will make an empty trie and [root_opt (Some (d, t))] will make a trie with only one binding: the root associated with the data [d] and the tag [t]. If the input is always [Some v], use {!val:root}. *)
val root_opt : ('data * 'tag) option -> ('data, 'tag) t

(** [prefix p t] makes a minimum trie with [t] rooted at [p]. *)
val prefix : path -> ('data, 'tag) t -> ('data, 'tag) t

(** [singleton (p, (d, t))] makes a trie with the only binding: [p] and its associated data [d] and tag [t]. It is equivalent to {!val:prefix}[ p @@ ]{!val:root}[ (d, t)] *)
val singleton : path * ('data * 'tag) -> ('data, 'tag) t

(** [equal eq_data eq_tag t1 t2] checks whether two tries are equal. *)
val equal : ('data -> 'data -> bool) -> ('tag -> 'tag -> bool) -> ('data, 'tag) t -> ('data, 'tag) t -> bool

(** {1 Finding Values} *)

(** [find_subtree p t] returns the subtree rooted at [p].

    @return The subtree with all the bindings under [p], including the binding at [p] itself (which will be the root). If there are no such bindings with the prefix [p], an empty trie is returned.
*)
val find_subtree : path -> ('data, 'tag) t -> ('data, 'tag) t

(** [find_singleton p t] returns the data and tag at [p]. *)
val find_singleton : path -> ('data, 'tag) t -> ('data * 'tag) option

(** [find_root t] returns the data and tag at the root. This is equivalent to {!val:find_singleton}[ [] t]. *)
val find_root : ('data, 'tag) t -> ('data * 'tag) option

(** {1 Mapping and Filtering} *)

(** [iter ~prefix f t] applies the function [f] to each data and tag in the trie.

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val iter : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> unit) -> ('data, 'tag) t -> unit

(** [map ~prefix f trie] applies the function [f] to each data and tag in the trie.

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val map : ?prefix:bwd_path -> (bwd_path -> 'data1 * 'tag1 -> 'data2 * 'tag2) -> ('data1, 'tag1) t -> ('data2, 'tag2) t

(** [filter ~prefix f trie] removes all data [d] with tag [t] at path [p] such that [f ~prefix:p (d, t)] returns [false].

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val filter : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> bool) -> ('data, 'tag) t -> ('data, 'tag) t

(** [filter_map ~prefix f trie] applies the function [f] to each data [d] with tag [t] at [p] in [trie]. If [f ~prefix:p (d, t)] returns [None], then the binding will be removed from the trie. Otherwise, if [f v] returns [Some d'], then the data will be replaced by [d'].

    @param prefix The prefix prepended to any path sent to [f]. The default is the empty prefix ([Emp]).
*)
val filter_map : ?prefix:bwd_path -> (bwd_path -> 'data1 * 'tag1 -> ('data2 * 'tag2) option) -> ('data1, 'tag1) t -> ('data2, 'tag2) t

(** {1 Updating} *)

(** [update_subtree p f t] replaces the subtree [t'] rooted at [p] in [t] with [f t']. *)
val update_subtree : path -> (('data, 'tag) t -> ('data, 'tag) t) -> ('data, 'tag) t -> ('data, 'tag) t

(** [update_singleton p f trie] replaces the data and tag at [p] in [trie] with the result of [f]. If there was no binding at [p], [f None] is evaluated. Otherwise, [f (Some (d, t))] is used where [d] and [t] are the data and the tag. If the result is [None], the old binding at [p] (if any) is removed. Otherwise, if the result is [Some (d', t')], the data and the tag at [p] are replaced by [d'] and [t']. *)
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

(** [union_singleton ~prefix merger t binding] is equivalent to {!val:union}[ ~prefix merger t1 (singleton binding)], but potentially more efficient.

    @param prefix The prefix prepended to any path sent to [merger]. The default is the empty prefix ([Emp]).
*)
val union_singleton : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag) -> ('data, 'tag) t -> path * ('data * 'tag) -> ('data, 'tag) t

(** [union_root ~prefix merger t r] is equivalent to {!val:union_singleton}[ ~prefix merger t ([], r)], but potentially more efficient.

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

(** [to_seq_with_bwd_paths] is like {!val:to_seq}, but with paths represented as backward lists.
    This is potentially more efficient than {!val:to_seq} because the conversion from a backward list to a forward list takes linear time.

    @param prefix The prefix prepended to any path in the output. The default is the empty prefix ([Emp]).
*)
val to_seq_with_bwd_paths : ?prefix:bwd_path -> ('data, 'tag) t -> (bwd_path * ('data * 'tag)) Seq.t

(** [to_seq_values t] traverses through the trie [t] in the lexicographical order but only returns the associated data and tags. This is potentially more efficient than {!val:to_seq} because the conversion of paths from backward lists to forward lists is skipped. *)
val to_seq_values : ('data, 'tag) t -> ('data * 'tag) Seq.t

(** [of_seq ~prefix s] inserts bindings [(p, d)] into an empty trie, one by one, using {!val:union_singleton}. Later bindings will shadow previous ones if the paths of bindings are not unique. *)
val of_seq : (path * ('data * 'tag)) Seq.t -> ('data, 'tag) t

(** [of_seq_with_merger ~prefix merger s] inserts bindings [(p, d)] into an empty trie, one by one, using {!val:union_singleton}. Bindings with the same path are resolved using [merger] instead of silent shadowing.

    @param prefix The prefix prepended to any path sent to [merger]. The default is the empty prefix ([Emp]). Note that [prefix] does not directly affect the output trie, only the argument to [merger].
*)
val of_seq_with_merger : ?prefix:bwd_path -> (bwd_path -> 'data * 'tag -> 'data * 'tag -> 'data * 'tag) -> (path * ('data * 'tag)) Seq.t -> ('data, 'tag) t

(** {1 Tags} *)

(** Untagged tries (where all tags are [()]). *)
type 'data untagged = ('data, unit) t

(** [retag tag t] changes all tags within [t] to [tag] in O(1) time. The data remain intact. *)
val retag : 'tag -> ('data, _) t -> ('data, 'tag) t

(** [retag_subtree tag path t] changes all tags within the subtrie rooted at [path] to [tag] efficiently. The data remain intact. *)
val retag_subtree : path -> 'tag -> ('data, 'tag) t -> ('data, 'tag) t

(** [untag t] is [retag () t]. *)
val untag : ('data, _) t -> 'data untagged

(** [set_of_tags t] returns the set of tags used in a trie, but as a [Seq.t]. *)
val set_of_tags : ('tag -> 'tag -> int) -> ('data, 'tag) t -> 'tag Seq.t
