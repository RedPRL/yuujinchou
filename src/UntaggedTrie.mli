(** @canonical Trie.Untagged *)

(** A convenience module {!module:Trie.Untagged} derived from {!module:Trie} for untagged tries {!type:Trie.untagged}. *)

type path = Trie.path

type bwd_path = Trie.bwd_path

type 'data t = 'data Trie.untagged
(** @canonical Trie.Untagged.t *)

val empty : 'data t

val is_empty : 'data t -> bool

val root : 'data -> 'data t

val root_opt : 'data option -> 'data t

val prefix : path -> 'data t -> 'data t

val singleton : path * 'data -> 'data t

val equal : ('data -> 'data -> bool) -> 'data t -> 'data t -> bool

val find_subtree : path -> 'data t -> 'data t

val find_singleton : path -> 'data t -> 'data option

val find_root : 'data t -> 'data option

val iter : ?prefix:bwd_path -> (bwd_path -> 'data -> unit) -> 'data t -> unit

val map : ?prefix:bwd_path -> (bwd_path -> 'data1 -> 'data2) -> 'data1 t -> 'data2 t

val filter : ?prefix:bwd_path -> (bwd_path -> 'data -> bool) -> 'data t -> 'data t

val filter_map : ?prefix:bwd_path -> (bwd_path -> 'data1 -> 'data2 option) -> 'data1 t -> 'data2 t

val update_subtree : path -> ('data t -> 'data t) -> 'data t -> 'data t

val update_singleton : path -> ('data option -> 'data option) -> 'data t -> 'data t

val update_root : ('data option -> 'data option) -> 'data t -> 'data t

val union : ?prefix:bwd_path -> (bwd_path -> 'data -> 'data -> 'data) -> 'data t -> 'data t -> 'data t

val union_subtree : ?prefix:bwd_path -> (bwd_path -> 'data -> 'data -> 'data) -> 'data t -> path * 'data t -> 'data t

val union_singleton : ?prefix:bwd_path -> (bwd_path -> 'data -> 'data -> 'data) -> 'data t -> path * 'data -> 'data t

val union_root : ?prefix:bwd_path -> (bwd_path -> 'data -> 'data -> 'data) -> 'data t -> 'data -> 'data t

val detach_subtree : path -> 'data t -> 'data t * 'data t

val detach_singleton : path -> 'data t -> 'data option * 'data t

val detach_root : 'data t -> 'data option * 'data t

val to_seq : ?prefix:bwd_path -> 'data t -> (path * 'data) Seq.t

val to_seq_with_bwd_paths : ?prefix:bwd_path -> 'data t -> (bwd_path * 'data) Seq.t

val to_seq_values : 'data t -> 'data Seq.t

val of_seq : (path * 'data) Seq.t -> 'data t

val of_seq_with_merger : ?prefix:bwd_path -> (bwd_path -> 'data -> 'data -> 'data) -> (path * 'data) Seq.t -> 'data t

val tag : 'tag -> 'data t -> ('data, 'tag) Trie.t

val untag : ('data, _) Trie.t -> 'data t
