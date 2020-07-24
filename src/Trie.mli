(** {1 Types} *)

(** The type of hierarchical names. The name [x.y.z] is represented by the OCaml list ["x"; "y"; "z"]. *)
type path = string list

(** The abstract type of a trie. *)
type +'a t

(** {1 Basic Operations} *)

(** The empty trie. *)
val empty : 'a t

(** Check whether the trie is empty. *)
val is_empty : 'a t -> bool

(** Make a trie with its root associated with the provided value. [mk_root None] will make an empty trie and [mk_root (Some v)] will make a trie with the value [v]. *)
val mk_root : 'a option -> 'a t

(** [prefix p t] makes a minimum trie with [t] rooted at [p]. *)
val prefix : path -> 'a t -> 'a t
(* val singleton : path * 'a -> 'a t *)
(* val root : 'a -> 'a t *)

(** [prefix p t] makes a minimum trie with [t] rooted at [p]. *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)

(* (** {1 Finding Data} *) *)
(* val find_subtree : path -> 'a t -> 'a t *)
(* val find_singleton : path -> 'a t -> 'a option *)

(** {1 Mapping and Filtering} *)

(* val map : ('a -> 'b) -> 'a t -> 'b t *)
(* val map_endo : ('a -> 'a) -> 'a t -> 'a t *)
(* val filter : ('a -> bool) -> 'a t -> 'a t *)
(* val filter_map : ('a -> 'b option) -> 'a t -> 'b t *)

(** [filter_map_endo f t] applies the function [f] to each value [v] in the trie. If [f v] returns [None], then the binding will be removed from the trie. Otherwise, if [f v] returns [Some v'], then the value will be replaced by [v'] in the returned trie. *)
val filter_map_endo : ('a -> 'a option) -> 'a t -> 'a t

(** {1 Union} *)

(** [union merger t1 t2] merges two tries [t1] and [t2]. If both tries have a binding at the same path, it will call the function [merger] to reconcile the values from the two tries. *)
val union : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

(** [union_subtree merger t1 (path, t2)] is equivalent to [union merger t1 (prefix t2)], but potentially more efficient. *)
val union_subtree : ('a -> 'a -> 'a) -> 'a t -> path * 'a t -> 'a t
(* val union_singleton : ('a -> 'a -> 'a) -> 'a t -> path * 'a -> 'a t *)

(* val update_subtree : path -> ('a t -> 'a t) -> 'a t -> 'a t *)
(* val update_singleton : path -> ('a option -> 'a option) -> 'a t -> 'a t *)

(** {1 Detach} *)

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
