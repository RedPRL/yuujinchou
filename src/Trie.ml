open StdLabels
open MoreLabels
open Bwd
open BwdNotation

type seg = string
type path = seg list
type bwd_path = seg bwd

module SegMap = Map.Make (String)

type 'a data_node = {
  root : 'a option;
  children : 'a data_node SegMap.t;
}

type 'a tag_node = {
  tag_default : 'a option;
  tag_root : 'a option;
  tag_children : 'a tag_node SegMap.t
}

let[@inline] get_tag_root t =
  match t.tag_root with
  | Some t -> t
  | None -> Option.get t.tag_default

let[@inline] tag_default_node tag_default : _ tag_node =
  {tag_default; tag_root = None; tag_children = SegMap.empty}

let[@inline] get_tag_child seg t : _ tag_node =
  match SegMap.find_opt seg t.tag_children with
  | Some t -> t
  | None -> tag_default_node t.tag_default

let[@inline] get_tag_root_opt t =
  match t.tag_root with
  | Some t -> Some t
  | None -> t.tag_default

type ('data, 'tag) node = 'data data_node * 'tag tag_node
(*
  Invariants:
  1. Without the default tags, the tag trie must be a subset of the data trie
  2. With the default tags, the tag trie must be a superset of the data trie
  3. Every default tag must be actually used by some data to prevent memory leak

  Non-invariants:
  1. The tag trie need not be minimum.
  2. This implementation prefers removing default tags. (See mk_node.)
*)

type ('data, 'tag) t = ('data, 'tag) node option

let empty : _ t = None

let is_empty : _ t -> bool = Option.is_none

let[@inline] non_empty (n : _ node) : _ t = Some n

(** {1 Making (non-empty) trees} *)

(* invariants: input tag tree must be a subset (if default tags were ignored) *)
let mk_tag_node n (tag_default, tag_root, tag_children) : _ tag_node =
  match tag_default with
  | Some tag_default when Int.equal (SegMap.cardinal n.children) (SegMap.cardinal tag_children) ->
    (* Remove unused default tags to prevent memory leak *)
    { tag_default = None;
      tag_root =
        (match n.root, tag_root with
         | Some _, None -> Some tag_default
         | _ -> tag_root);
      tag_children }
  | _ ->
    { tag_default; tag_root; tag_children }
let tag_to_params t = Some t, None, SegMap.empty
let mk_tag_node' n t : _ tag_node = mk_tag_node n @@ tag_to_params t
let mk_node n tag_params : _ node = n, mk_tag_node n tag_params
let mk_node' n tag_params : _ node = n, mk_tag_node' n tag_params

(* invariants: input tag tree must be a subset (if default tags were ignored) *)
let mk_tree (root, children) (tag_default, tag_root, tag_children) : _ t =
  if Option.is_none root && SegMap.is_empty children
  then empty
  else
    let n = {root; children} in
    non_empty @@ mk_node n (tag_default, tag_root, tag_children)

let[@inline] root_node (data, tag) =
  {root = Some data; children = SegMap.empty},
  {tag_default = None; tag_root = Some tag; tag_children = SegMap.empty}

let[@inline] root_opt v = Option.map root_node v

let[@inline] root v = non_empty @@ root_node v

let[@inline] prefix_node path n : _ node =
  let f seg (d, t) =
    {root = None; children = SegMap.singleton seg d},
    {tag_default = None; tag_root = None; tag_children = SegMap.singleton seg t}
  in
  List.fold_right ~f path ~init:n

let[@inline] prefix path = Option.map @@ prefix_node path

let[@inline] singleton (path, (d, t)) = prefix path (root (d, t))

(** {1 Equality} *)

let comb_d_t ds (def, ts) = SegMap.merge ds ts
    ~f:(fun _ d t ->
        match d, t with
        | None, _ -> None
        | Some d, None -> Some (d, tag_default_node def)
        | Some d, Some t -> Some (d, t))
let comb_d_t2 ds (def1, ts1) (def2, ts2) = SegMap.merge (comb_d_t ds (def1, ts1)) ts2
    ~f:(fun _ d_t1 t2 ->
        match d_t1, t2 with
        | None, _ -> None
        | Some (d, t1), None -> Some (d, t1, tag_default_node def2)
        | Some (d, t1), Some t2 -> Some (d, t1, t2))
let split_d_t combined = SegMap.map ~f:fst combined, SegMap.map ~f:snd combined
let split_option = function None -> None, None | Some (d, t) -> Some d, Some t

let equal_tag_default_node eq_tag t1 t2 =
  Option.equal eq_tag t1.tag_default t2.tag_default &&
  Option.is_none t1.tag_root && Option.is_none t2.tag_root &&
  SegMap.is_empty t1.tag_children && SegMap.is_empty t2.tag_children

let rec equal_tag_node d eq_tag t1 t2 =
  t1 == t2 ||
  equal_tag_default_node eq_tag t1 t2 ||
  (Option.is_none d.root || eq_tag (get_tag_root t1) (get_tag_root t2)) &&
  equal_tag_children d.children eq_tag (t1.tag_default, t1.tag_children) (t2.tag_default, t2.tag_children)

and equal_tag_children ds eq_tag (def_t1, ts1) (def_t2, ts2) =
  SegMap.for_all (comb_d_t2 ds (def_t1, ts1) (def_t2, ts2))
    ~f:(fun _ (d, t1, t2) -> equal_tag_node d eq_tag t1 t2)

let rec equal_data_node eq n1 n2 =
  n1 == n2 ||
  Option.equal eq n1.root n2.root &&
  SegMap.equal ~cmp:(equal_data_node eq) n1.children n2.children

let equal_node eq_data eq_tag (d1, t1) (d2, t2) =
  (d1 == d2 || equal_data_node eq_data d1 d2) &&
  (t1 == t2 || equal_tag_node d1 eq_tag t1 t2)

let equal eq_data eq_tag = Option.equal (equal_node eq_data eq_tag)

(** {1 Getting data} *)

let rec find_node_cont path (d, t) k =
  match path with
  | [] -> k (d, t)
  | seg::path ->
    Option.bind (SegMap.find_opt seg d.children) @@ fun d ->
    find_node_cont path (d, get_tag_child seg t) k

let find_subtree path v =
  Option.bind v @@ fun n -> find_node_cont path n non_empty

let find_root_node (d, t) = Option.map (fun r -> r, get_tag_root t) d.root

let find_singleton path v =
  Option.bind v @@ fun n -> find_node_cont path n find_root_node

let find_root v = Option.bind v find_root_node

(** {1 Updating} *)

let map_snd f (a, b) = a, f b

let rec apply_and_update_node_cont path (d, t) (k : (_, 'tag) t -> 'ans * (_, 'tag) t) =
  match path with
  | [] -> k @@ non_empty (d, t)
  | seg::path ->
    (match SegMap.find_opt seg d.children with
     | None -> map_snd (prefix path) @@ k empty
     | Some d -> apply_and_update_node_cont path (d, get_tag_child seg t) k)
    |> map_snd @@
    function
    | None -> (* removing the subtree *)
      mk_tree
        (d.root, SegMap.remove seg d.children)
        (t.tag_default, t.tag_root, SegMap.remove seg t.tag_children)
    | Some (d', t') -> (* replacing the subtree *)
      mk_tree
        (d.root, SegMap.add ~key:seg ~data:d' d.children)
        (t.tag_default, t.tag_root, SegMap.add ~key:seg ~data:t' t.tag_children)

let update_cont path v k =
  match v with
  | None -> prefix path @@ k empty
  | Some n -> snd (apply_and_update_node_cont path n (fun v -> (), k v))

let update_subtree path f v = update_cont path v f

let update_root f =
  function
  | None -> root_opt @@ f None
  | Some (d, t) ->
    match f (Option.map (fun r -> r, get_tag_root t) d.root) with
    | None -> mk_tree (None, d.children) (t.tag_default, None, t.tag_children)
    | Some (r, rt) -> mk_tree (Some r, d.children) (t.tag_default, Some rt, t.tag_children)

let update_singleton path f v = update_cont path v (update_root f)

(** {1 Union} *)

let union_root m (r1, rt1) (r2, rt2) =
  match r1, r2 with
  | None, None -> None, None
  | Some _, None -> r1, rt1
  | None, Some _ -> r2, rt2
  | Some r1, Some r2 ->
    let r, rt = m (r1, Option.get rt1) (r2, Option.get rt2) in
    Some r, Some rt

let rec union_node ~prefix m (d1, t1) (d2, t2) =
  let root, tag_root = union_root (m prefix) (d1.root, get_tag_root_opt t1) (d2.root, get_tag_root_opt t2) in
  let children, tag_children =
    let f seg n n' = Some (union_node ~prefix:(prefix #< seg) m n n') in
    split_d_t @@ SegMap.union ~f
      (comb_d_t d1.children (t1.tag_default, t1.tag_children))
      (comb_d_t d2.children (t2.tag_default, t2.tag_children))
  in
  {root; children}, {tag_default = None; tag_root; tag_children}

let union_ ~prefix m v1 v2 =
  match v1, v2 with
  | None, v | v, None -> v
  | Some n1, Some n2 ->
    Some (union_node ~prefix m n1 n2)

let[@inline] union ?(prefix=Emp) m = union_ ~prefix m

let union_subtree ?(prefix=Emp) m v1 (path, v2) =
  update_cont path v1 @@ fun v1 -> union_ ~prefix:(prefix <>< path) m v1 v2

let union_root ?(prefix=Emp) m v1 v2 =
  match v1 with
  | None -> root v2
  | Some (d1, t1) ->
    let root, tag_root = union_root (m prefix) (d1.root, get_tag_root_opt t1) (Some (fst v2), Some (snd v2)) in
    non_empty ({d1 with root}, {t1 with tag_root})

let union_singleton ?(prefix=Emp) m v1 (path, v2) =
  update_cont path v1 @@ fun v1 -> union_root ~prefix:(prefix <>< path) m v1 v2

(** {1 Detaching subtrees} *)

let apply_and_update_cont path t (k : _ t -> 'ans * _ t) : 'ans * _ t =
  match t with
  | None -> let ans, t = k empty in ans, prefix path t
  | Some n -> apply_and_update_node_cont path n k

let detach_subtree path t = apply_and_update_cont path t @@ fun t -> t, empty

let detach_root =
  function
  | None -> None, empty
  | Some (d, t) ->
    Option.map (fun r -> r, get_tag_root t) d.root,
    mk_tree (None, d.children) (t.tag_default, None, t.tag_children)

let detach_singleton path t = apply_and_update_cont path t detach_root

(** {1 Iteration} *)

let rec iter_node ~prefix f (d, t) =
  Option.fold ~none:() ~some:(fun r -> f prefix (r, get_tag_root t)) d.root;
  SegMap.iter (comb_d_t d.children (t.tag_default, t.tag_children))
    ~f:(fun ~key:seg ~data:(d, t) -> iter_node ~prefix:(prefix #< seg) f (d, t))
let iter ?(prefix=Emp) f v = Option.fold ~none:() ~some:(iter_node ~prefix f) v

let rec filter_map_node ~prefix f (d, t) : _ t =
  let root, tag_root = split_option @@ Option.bind d.root (fun r -> f prefix (r, get_tag_root t)) in
  let children, tag_children =
    split_d_t @@
    SegMap.filter_map (comb_d_t d.children (t.tag_default, t.tag_children))
      ~f:(fun seg -> filter_map_node ~prefix:(prefix #< seg) f)
  in
  mk_tree (root, children) (None, tag_root, tag_children)
let filter_map ?(prefix=Emp) f v = Option.bind v @@ filter_map_node ~prefix f

let map ?prefix f = filter_map ?prefix @@ fun prefix (d, t) -> Some (f prefix (d, t))

let filter ?prefix f = filter_map ?prefix @@
  fun prefix (d, t) -> if f prefix (d, t) then Some (d, t) else None

(** {1 Conversion from/to Seq} *)

let rec node_to_seq_with_bwd_paths ~prefix (d, t) () =
  let kont () = children_to_seq_with_bwd_paths ~prefix (d.children, t.tag_default, t.tag_children) () in
  match d.root with
  | None -> kont ()
  | Some v -> Seq.Cons ((prefix, (v, get_tag_root t)), kont)
and children_to_seq_with_bwd_paths ~prefix (ds, dt, ts) =
  SegMap.to_seq (comb_d_t ds (dt, ts)) |> Seq.flat_map @@ fun (seg, (d, t)) ->
  node_to_seq_with_bwd_paths ~prefix:(prefix #< seg) (d, t)

let to_seq_with_bwd_paths ?(prefix=Emp) t =
  Option.fold ~none:Seq.empty ~some:(node_to_seq_with_bwd_paths ~prefix) t

let to_seq_values t = Seq.map snd @@
  to_seq_with_bwd_paths t

let to_seq_data t = Seq.map (fun (_, (d, _)) -> d) @@
  to_seq_with_bwd_paths t

let to_seq_tags t = Seq.map (fun (_, (_, t)) -> t) @@
  to_seq_with_bwd_paths t

let to_seq ?prefix t = Seq.map (fun (p, v) -> Bwd.to_list p, v) @@
  to_seq_with_bwd_paths ?prefix t

let of_seq_with_merger ?prefix m = Seq.fold_left (union_singleton ?prefix m) empty

let of_seq s = of_seq_with_merger ~prefix:Emp (fun _ _ y -> y) s

(** {1 Tags} *)

let[@inline] retag t : _ t -> _ t =
  function
  | None -> None
  | Some (d, _) -> non_empty @@ mk_node' d t

let retag_subtree path t (v : _ t) : _ t = update_subtree path (retag t) v
