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
  default_tag : 'a option;
  root_tag : 'a option;
  child_tags : 'a tag_node SegMap.t
}

let[@inline] get_root_tag t =
  match t.root_tag with
  | Some t -> t
  | None -> Option.get t.default_tag

let[@inline] get_root_tag_opt t =
  match t.root_tag with
  | Some t -> Some t
  | None -> t.default_tag

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
let mk_tag_node n (default_tag, root_tag, child_tags) : _ tag_node =
  match default_tag with
  | Some default_tag when Int.equal (SegMap.cardinal n.children) (SegMap.cardinal child_tags) ->
    (* Remove unused default tags to prevent memory leak *)
    { default_tag = None;
      root_tag =
        (match n.root, root_tag with
         | Some _, None -> Some default_tag
         | _ -> root_tag);
      child_tags }
  | _ ->
    { default_tag; root_tag; child_tags }
let mk_tag_node' n t : _ tag_node = mk_tag_node n (Some t, None, SegMap.empty)
let mk_node n tag_params : _ node = n, mk_tag_node n tag_params
let mk_node' n tag_params : _ node = n, mk_tag_node' n tag_params

(* invariants: input tag tree must be a subset (if default tags were ignored) *)
let mk_tree (root, children) (default_tag, root_tag, child_tags) : _ t =
  if Option.is_none root && SegMap.is_empty children
  then empty
  else
    let n = {root; children} in
    non_empty @@ mk_node n (default_tag, root_tag, child_tags)

let[@inline] root_node (data, tag) =
  {root = Some data; children = SegMap.empty},
  {default_tag = None; root_tag = Some tag; child_tags = SegMap.empty}

let[@inline] root_opt v = Option.map root_node v

let[@inline] root v = non_empty @@ root_node v

let[@inline] prefix_node path n : _ node =
  let f seg (d, t) =
    {root = None; children = SegMap.singleton seg d},
    {default_tag = None; root_tag = None; child_tags = SegMap.singleton seg t}
  in
  List.fold_right ~f path ~init:n

let[@inline] prefix path = Option.map @@ prefix_node path

let[@inline] singleton (path, (d, t)) = prefix path (root (d, t))

(** {1 Equality} *)

let comb_d_t ds ts = SegMap.merge ~f:(fun _ d t -> Option.map (fun d -> d, t) d) ds ts
let comb_self m1 m2 = SegMap.merge ~f:(fun _ x y -> Some (x, y)) m1 m2
let split_d_t combined = SegMap.map ~f:fst combined, SegMap.map ~f:snd combined
let split_option = function None -> None, None | Some (d, t) -> Some d, Some t

let rec equal_node_1d1t eq_tag d t2 =
  (Option.is_none d.root || eq_tag (get_root_tag t2)) &&
  equal_children_1d1t eq_tag d.children (t2.default_tag, t2.child_tags)

and equal_children_1d1t eq_tag ds (def_t2, ts2) =
  let default_tag_matched = lazy (eq_tag (Option.get def_t2)) in
  SegMap.for_all (comb_d_t ds ts2) ~f:(fun _ ->
      function
      | _, None -> Lazy.force default_tag_matched
      | d, Some t2 -> equal_node_1d1t eq_tag d t2)

let rec equal_node_1d eq_tag d t1 t2 =
  t1 == t2 ||
  (Option.is_none d.root || eq_tag (get_root_tag t1) (get_root_tag t2)) &&
  equal_children_1d eq_tag d.children (t1.default_tag, t1.child_tags) (t2.default_tag, t2.child_tags)

and equal_children_1d eq_tag ds (def_t1, ts1) (def_t2, ts2) =
  let default_tag_matched = lazy (eq_tag (Option.get def_t1) (Option.get def_t2)) in
  SegMap.for_all (comb_d_t ds (comb_self ts1 ts2)) ~f:(fun _ ->
      function
      | _, (None | Some (None, None)) -> Lazy.force default_tag_matched
      | d, Some (None, Some t2) -> equal_node_1d1t (eq_tag (Option.get def_t1)) d t2
      | d, Some (Some t1, None) -> equal_node_1d1t (Fun.flip eq_tag (Option.get def_t2)) d t1
      | d, Some (Some t1, Some t2) -> equal_node_1d eq_tag d t1 t2)

let rec equal_data_node eq n1 n2 =
  n1 == n2 ||
  Option.equal eq n1.root n2.root &&
  SegMap.equal ~cmp:(equal_data_node eq) n1.children n2.children

let rec equal_node_1t eq_data eq_tag d1 (d2, t2) =
  Option.equal (fun r1 r2 -> eq_data r1 r2 && eq_tag (get_root_tag t2)) d1.root d2.root &&
  equal_children_1t eq_data eq_tag d1.children (d2.children, t2.default_tag, t2.child_tags)

and equal_children_1t eq_data eq_tag ds1 (ds2, def_t2, ts2) =
  let default_tag_matched = lazy (eq_tag (Option.get def_t2)) in
  SegMap.for_all (comb_d_t (comb_self ds1 ds2) ts2) ~f:(fun _ ->
      function
      | (None, None), _ -> true
      | (Some _, None), _ | (None, Some _), _ -> false
      | (Some d1, Some d2), None ->
        Lazy.force default_tag_matched && equal_data_node eq_data d1 d2
      | (Some d1, Some d2), Some t2 ->
        equal_node_1t eq_data eq_tag d1 (d2, t2))

let rec equal_node eq_data eq_tag (d1, t1) (d2, t2) =
  d1 == d2 && equal_node_1d eq_tag d1 t1 t2 ||
  Option.equal (fun r1 r2 -> eq_data r1 r2 && eq_tag (get_root_tag t1) (get_root_tag t2)) d1.root d2.root &&
  equal_children eq_data eq_tag (d1.children, t1.default_tag, t1.child_tags) (d2.children, t2.default_tag, t2.child_tags)

and equal_children eq_data eq_tag (ds1, def_t1, ts1) (ds2, def_t2, ts2) =
  let default_tag_matched = lazy (eq_tag (Option.get def_t1) (Option.get def_t2)) in
  SegMap.for_all (comb_d_t (comb_self ds1 ds2) (comb_self ts1 ts2)) ~f:(fun _ ->
      function
      | (None, None), _ -> true
      | (Some _, None), _ | (None, Some _), _ -> false
      | (Some d1, Some d2), (None | Some (None, None)) ->
        Lazy.force default_tag_matched && equal_data_node eq_data d1 d2
      | (Some d1, Some d2), Some (None, Some t2) ->
        equal_node_1t eq_data (eq_tag (Option.get def_t1)) d1 (d2, t2)
      | (Some d1, Some d2), Some (Some t1, None) ->
        equal_node_1t (Fun.flip eq_data) (Fun.flip eq_tag (Option.get def_t2)) d2 (d1, t1)
      | (Some d1, Some d2), Some (Some t1, Some t2) ->
        equal_node eq_data eq_tag (d1, t1) (d2, t2))

let equal eq_data eq_tag = Option.equal (equal_node eq_data eq_tag)

(** {1 Getting data} *)

let rec find_data_node_cont path (d, def_t) k =
  match path with
  | [] -> k @@ mk_node' d def_t
  | seg::path ->
    Option.bind (SegMap.find_opt seg d.children) @@ fun d ->
    find_data_node_cont path (d, def_t) k

let rec find_node_cont path (d, t) k =
  match path with
  | [] -> k (d, t)
  | seg::path ->
    Option.bind (SegMap.find_opt seg d.children) @@ fun d ->
    match SegMap.find_opt seg t.child_tags with
    | None -> find_data_node_cont path (d, Option.get t.default_tag) k
    | Some t -> find_node_cont path (d, t) k

let find_subtree path v =
  Option.bind v @@ fun n -> find_node_cont path n non_empty

let find_root_node (d, t) = Option.map (fun r -> r, get_root_tag t) d.root

let find_singleton path v =
  Option.bind v @@ fun n -> find_node_cont path n find_root_node

let find_root v = Option.bind v find_root_node

(** {1 Updating} *)

let rec apply_and_update_data_node_cont path (d, (def_t : 'tag)) (k : (_, 'tag) t -> 'ans * (_, 'tag) t) =
  match path with
  | [] -> k @@ non_empty @@ mk_node' d def_t
  | seg::path ->
    match
      match SegMap.find_opt seg d.children with
      | None -> let ans, v = k empty in ans, prefix path v
      | Some d -> apply_and_update_data_node_cont path (d, def_t) k
    with
    | ans, None ->
      ans, mk_tree (d.root, SegMap.remove seg d.children) (Some def_t, None, SegMap.empty)
    | ans, Some (d', t') ->
      ans, mk_tree (d.root, SegMap.add ~key:seg ~data:d' d.children) (Some def_t, None, SegMap.singleton seg t')

let rec apply_and_update_node_cont path (d, (t : 'tag tag_node)) (k : (_, 'tag) t -> 'ans * (_, 'tag) t) =
  match path with
  | [] -> k @@ non_empty (d, t)
  | seg::path ->
    match
      match SegMap.find_opt seg d.children with
      | None -> let ans, v = k empty in ans, prefix path v
      | Some d ->
        match SegMap.find_opt seg t.child_tags with
        | None -> apply_and_update_data_node_cont path (d, Option.get t.default_tag) k
        | Some t -> apply_and_update_node_cont path (d, t) k
    with
    | ans, None ->
      ans, mk_tree (d.root, SegMap.remove seg d.children) (t.default_tag, t.root_tag, SegMap.remove seg t.child_tags)
    | ans, Some (d', t') ->
      ans,
      mk_tree (d.root, SegMap.add ~key:seg ~data:d' d.children) (t.default_tag, t.root_tag, SegMap.add ~key:seg ~data:t' t.child_tags)

let update_cont path v k =
  match v with
  | None -> prefix path @@ k empty
  | Some n -> snd (apply_and_update_node_cont path n (fun v -> (), k v))

let update_subtree path f v = update_cont path v f

let update_root f =
  function
  | None -> root_opt @@ f None
  | Some (d, t) ->
    match f (Option.map (fun r -> r, get_root_tag t) d.root) with
    | None -> mk_tree (None, d.children) (t.default_tag, None, t.child_tags)
    | Some (r, rt) -> mk_tree (Some r, d.children) (t.default_tag, Some rt, t.child_tags)

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
  let root, root_tag = union_root (m prefix) (d1.root, get_root_tag_opt t1) (d2.root, get_root_tag_opt t2) in
  let children, child_tags =
    let augment ds dt ts = SegMap.merge ds ts ~f:(fun _ d t ->
        match d, t with
        | None, _ -> None
        | Some d, None -> non_empty @@ mk_node' d (Option.get dt)
        | Some d, Some t -> non_empty (d, t))
    in
    let augmented1 = augment d1.children t1.default_tag t1.child_tags
    and augmented2 = augment d2.children t2.default_tag t2.child_tags
    in
    let f seg n n' = Some (union_node ~prefix:(prefix #< seg) m n n') in
    split_d_t @@ SegMap.union ~f augmented1 augmented2
  in
  {root; children}, {default_tag = None; root_tag; child_tags}

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
    let root, root_tag = union_root (m prefix) (d1.root, get_root_tag_opt t1) (Some (fst v2), Some (snd v2)) in
    non_empty ({d1 with root}, {t1 with root_tag})

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
    Option.map (fun r -> r, get_root_tag t) d.root,
    mk_tree (None, d.children) (t.default_tag, None, t.child_tags)

let detach_singleton path t = apply_and_update_cont path t detach_root

(** {1 Iteration} *)

let rec iter_data_node ~prefix f (d, dt) =
  Option.fold ~none:() ~some:(fun r -> f prefix (r, dt)) d.root;
  SegMap.iter ~f:(fun ~key ~data -> iter_data_node ~prefix:(prefix #< key) f (data, dt)) d.children
let rec iter_node ~prefix f (d, t) =
  Option.fold ~none:() ~some:(fun r -> f prefix (r, get_root_tag t)) d.root;
  SegMap.iter (comb_d_t d.children t.child_tags)
    ~f:(fun ~key:seg ~data ->
        match data with
        | d, None -> iter_data_node ~prefix:(prefix #< seg) f (d, Option.get t.default_tag)
        | d, Some t -> iter_node ~prefix:(prefix #< seg) f (d, t))
let iter ?(prefix=Emp) f v = Option.fold ~none:() ~some:(iter_node ~prefix f) v

let rec filter_map_data_node ~prefix f (d, dt) =
  let root, root_key = split_option @@ Option.bind d.root @@ fun r -> f prefix (r, dt) in
  let children, child_tags =
    split_d_t @@
    SegMap.filter_map
      ~f:(fun seg d -> filter_map_data_node ~prefix:(prefix #< seg) f (d, dt))
      d.children
  in
  mk_tree (root, children) (None, root_key, child_tags)
let rec filter_map_node ~prefix f (d, t) : _ t =
  let root, root_tag = split_option @@ Option.bind d.root (fun r -> f prefix (r, get_root_tag t)) in
  let children, child_tags =
    split_d_t @@
    SegMap.filter_map (comb_d_t d.children t.child_tags)
      ~f:(fun seg ->
          function
          | d, None -> filter_map_data_node ~prefix:(prefix #< seg) f (d, Option.get t.default_tag)
          | d, Some t -> filter_map_node ~prefix:(prefix #< seg) f (d, t))
  in
  mk_tree (root, children) (None, root_tag, child_tags)
let filter_map ?(prefix=Emp) f v = Option.bind v @@ filter_map_node ~prefix f

let map ?prefix f = filter_map ?prefix @@ fun prefix (d, t) -> Some (f prefix (d, t))

let filter ?prefix f = filter_map ?prefix @@
  fun prefix (d, t) -> if f prefix (d, t) then Some (d, t) else None

(** {1 Conversion from/to Seq} *)

let rec data_node_to_seq_with_bwd_paths ~prefix (d, dt) () =
  let kont () = data_children_to_seq_with_bwd_paths ~prefix (d.children, dt) () in
  match d.root with
  | None -> kont ()
  | Some v -> Seq.Cons ((prefix, (v, dt)), kont)
and data_children_to_seq_with_bwd_paths ~prefix (ds, dt) =
  SegMap.to_seq ds |> Seq.flat_map @@ fun (seg, d) ->
  data_node_to_seq_with_bwd_paths ~prefix:(prefix #< seg) (d, dt)

let rec node_to_seq_with_bwd_paths ~prefix (d, t) () =
  let kont () = children_to_seq_with_bwd_paths ~prefix (d.children, t.default_tag, t.child_tags) () in
  match d.root with
  | None -> kont ()
  | Some v -> Seq.Cons ((prefix, (v, get_root_tag t)), kont)
and children_to_seq_with_bwd_paths ~prefix (ds, dt, ts) =
  SegMap.to_seq (comb_d_t ds ts) |> Seq.flat_map @@
  function
  | seg, (d, None) -> data_node_to_seq_with_bwd_paths ~prefix:(prefix #< seg) (d, Option.get dt)
  | seg, (d, Some t) -> node_to_seq_with_bwd_paths ~prefix:(prefix #< seg) (d, t)

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
