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

(** Make a trie with only one binding: the root associated with the provided value. [mk_root None] will make an empty trie and [mk_root (Some v)] will make a trie with the value [v]. If thi input is always [Some v], use {!val:root}. *)
val mk_root : 'a option -> 'a t

(** [prefix p t] makes a minimum trie with [t] rooted at [p]. *)
val prefix : path -> 'a t -> 'a t

(** [strington (p, d)] makes a trie with the only binding: [p] and its associated value [d]. *)
val singleton : path * 'a -> 'a t

(** [root d] makes a trie with the only binding: the root and its associated value [d]. It is equivalent to [singleton [] d]. *)
val root : 'a -> 'a t

(** [equal eq t1 t2] checks whether two tries are equal. It the internal representations of tries are physically equal, [equal eq t1 t2] will return [true] without calling [eq]. *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)

(** {1 Locating Values} *)

(** [find_subtree p t] returns the subtree rooted at [p]. *)
val find_subtree : path -> 'a t -> 'a t

(** [find_singleton p t] returns the value at [p]. *)
val find_singleton : path -> 'a t -> 'a option

(** [find_root t] returns the value at the root. This is equivalent to [find_singleton [] t]. *)
val find_root : 'a t -> 'a option

(** {1 Mapping and Filtering} *)

(** [map f t] applies the function [f] to each value [v] in the trie. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(* val map_endo : ('a -> 'a) -> 'a t -> 'a t *)
(* val filter : ('a -> bool) -> 'a t -> 'a t *)
(* val filter_map : ('a -> 'b option) -> 'a t -> 'b t *)

(** [filter_map_endo f t] applies the function [f] to each value [v] in the trie. If [f v] returns [None], then the binding will be removed from the trie. Otherwise, if [f v] returns [Some v'], then the value will be replaced by [v'] in the returned trie. *)
val filter_map_endo : ('a -> 'a option) -> 'a t -> 'a t

(** {1 Updating} *)

(** [update_subtree p f t] replaces the subtree [t'] rooted at [p] in [t] with [f t']. It will try to preserve physical equality when [f] returns the trie unchanged. *)
val update_subtree : path -> ('a t -> 'a t) -> 'a t -> 'a t

(** [update_singleton p f t] replaces the value [v] at [p] in [t] with the result of [f]. If there was no binding at [p], [f None] is evaluated. Otherwise, [f (Some v)] is used. If the result is [None], the old binding at [p] (if any) is removed. Otherwise, if the result is [Some v'], the value at [p] is replaced by [v']. It will try to preserve physical equality when [f] maintains the current status of binding, either returning [None] for [None] or [Some v] for [Some v]. *)
val update_singleton : path -> ('a option -> 'a option) -> 'a t -> 'a t

(** [update_root f t] updates the value at root with [f]. It is equivalent to [update_singleton [] f t]. *)
val update_root : ('a option -> 'a option) -> 'a t -> 'a t

(** {1 Union} *)

(** [union merger t1 t2] merges two tries [t1] and [t2]. If both tries have a binding at the same path, it will call the function [merger] to reconcile the values from the two tries. *)
val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

(** [union_subtree merger t1 (path, t2)] is equivalent to [union merger t1 (prefix t2)], but potentially more efficient. *)
val union_subtree : ('a -> 'a -> 'a) -> 'a t -> path * 'a t -> 'a t

(** [union_singleton merger t binding] is equivalent to [union merger t1 (singleton binding)], but potentially more efficient. *)
val union_singleton : ('a -> 'a -> 'a) -> 'a t -> path * 'a -> 'a t

(** {1 Separation} *)

(** [detach p t] detaches the subtree at [p] from the main trie and returns both the subtree and the remaining trie. If [detach p t] returns [t1, t2], then [union_subtree m t2 (p, t1)] should be equivalent to [t]. *)
val detach_subtree : path -> 'a t -> 'a t * 'a t

(** [detach p t] detaches the binding at [p] from the main trie and returns both the binding and the remaining trie. If [detach p t] returns [b, t'], then [union_subtree m t' (p, mk_root b)] should be equivalent to [t]. *)
val detach_singleton : path -> 'a t -> 'a option * 'a t

(** {1 Iterators} *)

(** [to_seq t] traverses through the trie [t] in the lexicographical order. *)
val to_seq : 'a t -> (path * 'a) Seq.t

(** [to_seq_values t] traverses through the trie [t] in the lexicographical order but only returns the associated values. This is faster than [Seq.map snd @@ to_seq t] because it does not need to reconstruct the paths. *)
val to_seq_values : 'a t -> 'a Seq.t

(** [of_seq m s] inserts bindings [(p, d)] into an empty trie, one by one, using {!val:union_subtree}. *)
val of_seq : ('a -> 'a -> 'a) -> (path * 'a) Seq.t -> 'a t

(** {1 Pretty Printer} *)

(** [pp pp_v t] prints out the content of [t], using the pretty printer [pp_v] on values. *)
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** {1 Physical Equality} *)

(** This is an internal API for testing whether the library is preserving physical equality as much as possible. If [physically_equal t1 t2] returns [true] then [equal eq t1 t2] must return [true]. Do not rely on this function unless you know the internals of tries. *)
val physically_equal : 'a t -> 'a t -> bool
