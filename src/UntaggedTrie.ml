(** A convenience module {!module:UntaggedTrie} derived from {!module:Trie} with the type of tags being {!type:unit}. *)

open Trie

type path = Trie.path
type bwd_path = Trie.bwd_path
type 'data t = ('data, unit) Trie.t

let[@inline] tag d = d, ()
let[@inline] tag_opt v = Option.map tag v
let[@inline] untag (d, ()) = d
let[@inline] untag_opt v = Option.map untag v
let[@inline] tag_lift f = (fun p x -> tag (f p (untag x)))
let[@inline] tag_lift2 f = (fun p x y -> tag (f p (untag x) (untag y)))

let empty : _ t = empty

let is_empty : _ t -> bool = is_empty

let root d = root (tag d)

let root_opt d = root_opt (tag_opt d)

let prefix : path -> _ t -> _ t = prefix

let singleton (p, d) = singleton (p, tag d)

let equal eq_data t1 t2 = equal eq_data Unit.equal t1 t2

let find_subtree : path -> _ t -> _ t = find_subtree

let find_singleton path (v : _ t) : _ option = untag_opt @@ find_singleton path v

let find_root (v : _ t) : _ option = untag_opt @@ find_root v

let iter ?prefix f : _ t -> unit = iter ?prefix (fun p d -> f p (untag d))

let map ?prefix f : _ t -> _ t = map ?prefix (tag_lift f)

let filter ?prefix f : _ t -> _ t = filter ?prefix (fun p d -> f p (untag d))

let filter_map ?prefix f : _ t -> _ t = filter_map ?prefix (fun p d -> tag_opt (f p (untag d)))

let update_subtree : path -> (_ t -> _ t) -> _ t -> _ t = update_subtree

let update_singleton path f : _ t -> _ t = update_singleton path (fun od -> tag_opt (f (untag_opt od)))

let update_root f : _ t -> _ t = update_root (fun od -> tag_opt (f (untag_opt od)))

let union ?prefix m : _ t -> _ t -> _ t = union ?prefix (tag_lift2 m)

let union_subtree ?prefix m : _ t -> path * _ t -> _ t = union_subtree ?prefix (tag_lift2 m)

let union_singleton ?prefix m t1 (path, d) : _ t = union_singleton ?prefix (tag_lift2 m) t1 (path, tag d)

let union_root ?prefix m t1 d : _ t = union_root ?prefix (tag_lift2 m) t1 (tag d)

let detach_subtree : path -> _ t -> _ t * _ t = detach_subtree

let detach_singleton path t : _ option * _ t = let d, t = detach_singleton path t in untag_opt d, t

let detach_root t : _ option * _ t = let d, t = detach_root t in untag_opt d, t

let to_seq ?prefix (v : _ t) : _ Seq.t = Seq.map (fun (p, d) -> p, (untag d)) @@ to_seq ?prefix v

let to_seq_with_bwd_paths ?prefix (v : _ t) : _ Seq.t = Seq.map (fun (p, d) -> p, (untag d)) @@ to_seq_with_bwd_paths ?prefix v

let to_seq_values : _ t -> _ Seq.t = to_seq_values

let of_seq s : _ t = of_seq @@ Seq.map (fun (p, d) -> p, tag d) s

let of_seq_with_merger ?prefix m s : _ t = of_seq_with_merger ?prefix (tag_lift2 m) @@ Seq.map (fun (p, d) -> p, tag d) s

let tag t = Trie.retag t

let untag v = Trie.retag () v
