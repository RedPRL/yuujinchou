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
  default_tag : 'a;
  root_tag : 'a option;
  child_tags : 'a tag_node SegMap.t
}

type ('data, 'tag) node = 'data data_node * 'tag tag_node

type ('data, 'tag) t = ('data, 'tag) node option

let empty : _ t = None

let is_empty : _ t -> bool = Option.is_none

let[@inline] non_empty (n : _ node) : _ t = Some n

(** {1 Making (non-empty) trees} *)

let[@inline] is_empty_ root children = Option.is_none root && SegMap.is_empty children

let mk_root_tag dt (r, rt) =
  if Option.is_none r || rt == dt then None else Some rt

let mk_tree (root, children) t : _ t =
  if is_empty_ root children then empty
  else non_empty ({root; children}, t)

let mk_data_tree (root, children) : _ data_node option =
  if is_empty_ root children then None
  else Some {root; children}

let[@inline] root_data_node data =
  {root = Some data; children = SegMap.empty}

let[@inline] default_tag_node default_tag =
  {default_tag; root_tag = None; child_tags = SegMap.empty}

let[@inline] is_default_tag_node default_tag t =
  t.default_tag == default_tag && Option.is_none t.root_tag && SegMap.is_empty t.child_tags

let[@inline] get_root_tag t = Option.value ~default:t.default_tag t.root_tag

let[@inline] root_node (data, tag) =
  root_data_node data, default_tag_node tag

let[@inline] root_opt v = Option.map root_node v

let[@inline] root v = non_empty @@ root_node v

let prefix_data_node path n : _ data_node =
  let f seg n = {root = None; children = SegMap.singleton seg n} in
  List.fold_right ~f path ~init:n

let prefix1_tag_node default_tag seg n : _ tag_node =
  if is_default_tag_node default_tag n
  then default_tag_node default_tag
  else {default_tag; root_tag = None; child_tags = SegMap.singleton seg n}

let prefix_tag_node default_tag path n : _ tag_node =
  List.fold_right ~f:(prefix1_tag_node default_tag) path ~init:n

let[@inline] prefix_node path (d, t) : _ node =
  prefix_data_node path d, prefix_tag_node t.default_tag path t

let[@inline] prefix path = Option.map @@ prefix_node path

let[@inline] singleton_data_node (path, d) = prefix_data_node path @@ root_data_node d

let[@inline] singleton (path, (d, t)) = non_empty (singleton_data_node (path, d), default_tag_node t)

(** {1 Comparison} *)

let comb_d_t ds ts = SegMap.merge ~f:(fun _ d t -> Option.map (fun d -> d, t) d) ds ts
let comb_self m1 m2 = SegMap.merge ~f:(fun _ x y -> Some (x, y)) m1 m2

let rec equal_node_1d1t eq_tag d t2 =
  (Option.is_none d.root || eq_tag (get_root_tag t2)) &&
  equal_children_1d1t eq_tag d.children (t2.default_tag, t2.child_tags)

and equal_children_1d1t eq_tag ds (def_t2, ts2) =
  let default_tag_matched = eq_tag def_t2 in
  SegMap.for_all (comb_d_t ds ts2) ~f:(fun _ ->
      function
      | _, None -> default_tag_matched
      | d, Some t2 -> equal_node_1d1t eq_tag d t2)

let rec equal_node_1d eq_tag d t1 t2 =
  t1 == t2 ||
  (Option.is_none d.root || eq_tag (get_root_tag t1) (get_root_tag t2)) &&
  equal_children_1d eq_tag d.children (t1.default_tag, t1.child_tags) (t2.default_tag, t2.child_tags)

and equal_children_1d eq_tag ds (def_t1, ts1) (def_t2, ts2) =
  let default_tag_matched = eq_tag def_t1 def_t2 in
  SegMap.for_all (comb_d_t ds (comb_self ts1 ts2)) ~f:(fun _ ->
      function
      | _, (None | Some (None, None)) -> default_tag_matched
      | d, Some (None, Some t2) -> equal_node_1d1t (eq_tag def_t1) d t2
      | d, Some (Some t1, None) -> equal_node_1d1t (Fun.flip eq_tag def_t2) d t1
      | d, Some (Some t1, Some t2) -> equal_node_1d eq_tag d t1 t2)

let rec equal_data_node eq n1 n2 =
  n1 == n2 ||
  Option.equal eq n1.root n2.root &&
  SegMap.equal ~cmp:(equal_data_node eq) n1.children n2.children

let rec equal_node_1t eq_data eq_tag d1 (d2, t2) =
  Option.equal (fun r1 r2 -> eq_data r1 r2 && eq_tag (get_root_tag t2)) d1.root d2.root &&
  equal_children_1t eq_data eq_tag d1.children (d2.children, t2.default_tag, t2.child_tags)

and equal_children_1t eq_data eq_tag ds1 (ds2, def_t2, ts2) =
  let default_tag_matched = eq_tag def_t2 in
  SegMap.for_all (comb_d_t (comb_self ds1 ds2) ts2) ~f:(fun _ ->
      function
      | (None, None), _ -> true
      | (Some _, None), _ | (None, Some _), _ -> false
      | (Some d1, Some d2), None ->
        default_tag_matched && equal_data_node eq_data d1 d2
      | (Some d1, Some d2), Some t2 ->
        equal_node_1t eq_data eq_tag d1 (d2, t2))

let rec equal_node eq_data eq_tag (d1, t1) (d2, t2) =
  d1 == d2 && equal_node_1d eq_tag d1 t1 t2 ||
  Option.equal (fun r1 r2 -> eq_data r1 r2 && eq_tag (get_root_tag t1) (get_root_tag t2)) d1.root d2.root &&
  equal_children eq_data eq_tag (d1.children, t1.default_tag, t1.child_tags) (d2.children, t2.default_tag, t2.child_tags)

and equal_children eq_data eq_tag (ds1, def_t1, ts1) (ds2, def_t2, ts2) =
  let default_tag_matched = eq_tag def_t1 def_t2 in
  SegMap.for_all (comb_d_t (comb_self ds1 ds2) (comb_self ts1 ts2)) ~f:(fun _ ->
      function
      | (None, None), _ -> true
      | (Some _, None), _ | (None, Some _), _ -> false
      | (Some d1, Some d2), (None | Some (None, None)) ->
        default_tag_matched && equal_data_node eq_data d1 d2
      | (Some d1, Some d2), Some (None, Some t2) ->
        equal_node_1t eq_data (eq_tag def_t1) d1 (d2, t2)
      | (Some d1, Some d2), Some (Some t1, None) ->
        equal_node_1t (Fun.flip eq_data) (Fun.flip eq_tag def_t2) d2 (d1, t1)
      | (Some d1, Some d2), Some (Some t1, Some t2) ->
        equal_node eq_data eq_tag (d1, t1) (d2, t2))

let equal eq_data eq_tag = Option.equal (equal_node eq_data eq_tag)

(** {1 Getting data} *)

let rec find_data_node_cont path (d, def_t) k =
  match path with
  | [] -> k (d, default_tag_node def_t)
  | seg::path ->
    Option.bind (SegMap.find_opt seg d.children) @@ fun d ->
    find_data_node_cont path (d, def_t) k

let rec find_node_cont path (d, t) k =
  match path with
  | [] -> k (d, t)
  | seg::path ->
    Option.bind (SegMap.find_opt seg d.children) @@ fun d ->
    match SegMap.find_opt seg t.child_tags with
    | None -> find_data_node_cont path (d, t.default_tag) k
    | Some t -> find_node_cont path (d, t) k

let find_subtree path v =
  Option.bind v @@ fun n -> find_node_cont path n non_empty

let find_root_node (d, t) = Option.map (fun r -> r, get_root_tag t) d.root

let find_singleton path v =
  Option.bind v @@ fun n -> find_node_cont path n find_root_node

let find_root v = Option.bind v find_root_node

(** {1 Updating} *)

let rec apply_and_update_data_node_cont path (d, def_t) (k : _ t -> 'ans * _ t) =
  match path with
  | [] -> k @@ non_empty (d, default_tag_node def_t)
  | seg::path ->
    match
      match SegMap.find_opt seg d.children with
      | None -> let ans, v = k empty in ans, prefix path v
      | Some d -> apply_and_update_data_node_cont path (d, def_t) k
    with
    | ans, None ->
      ans, mk_tree (d.root, SegMap.remove seg d.children) (default_tag_node def_t)
    | ans, Some (d', t') ->
      ans, mk_tree (d.root, SegMap.add ~key:seg ~data:d' d.children) (prefix1_tag_node def_t seg t')

let rec apply_and_update_node_cont path (d, t) (k : _ t -> 'ans * _ t) =
  match path with
  | [] -> k @@ non_empty (d, t)
  | seg::path ->
    match
      match SegMap.find_opt seg d.children with
      | None -> let ans, v = k empty in ans, prefix path v
      | Some d ->
        match SegMap.find_opt seg t.child_tags with
        | None -> apply_and_update_data_node_cont path (d, t.default_tag) k
        | Some t -> apply_and_update_node_cont path (d, t) k
    with
    | ans, None ->
      ans, mk_tree (d.root, SegMap.remove seg d.children) {t with child_tags = SegMap.remove seg t.child_tags}
    | ans, Some (d', t') ->
      ans,
      mk_tree (d.root, SegMap.add ~key:seg ~data:d' d.children)
        (if is_default_tag_node t.default_tag t'
         then {t with child_tags = SegMap.remove seg t.child_tags}
         else {t with child_tags = SegMap.add ~key:seg ~data:t' t.child_tags})

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
    | None -> mk_tree (None, d.children) {t with root_tag = None}
    | Some (r, rt) ->
      mk_tree (Some r, d.children) {t with root_tag = mk_root_tag t.default_tag (Some r, rt)}

let update_singleton path f v = update_cont path v (update_root f)

(** {1 Union} *)

let union_root default_tag m r1 r2 =
  let r, rt =
    match r1, r2 with
    | r, (None, _) | (None, _), r -> r
    | (Some r1, rt1), (Some r2, rt2) ->
      let r, rt = m (r1, rt1) (r2, rt2) in Some r, rt
  in
  r, mk_root_tag default_tag (r, rt)

let split_d_t default_tag combined =
  SegMap.map ~f:fst combined,
  SegMap.filter_map combined ~f:(fun _ (_, t) -> if is_default_tag_node default_tag t then None else Some t)

let rec union_node ~prefix m (d1, t1) (d2, t2) =
  let default_tag =
    if SegMap.cardinal d1.children >= SegMap.cardinal d2.children
    then t1.default_tag
    else t2.default_tag
  in
  let root, root_tag = union_root default_tag (m prefix) (d1.root, get_root_tag t1) (d2.root, get_root_tag t2) in
  let children, child_tags =
    let augment ds dt ts = SegMap.merge ds ts ~f:(fun _ d t -> Option.map (fun d -> d, Option.value ~default:dt t) d) in
    let augmented1 = augment d1.children (default_tag_node t1.default_tag) t1.child_tags
    and augmented2 = augment d2.children (default_tag_node t2.default_tag) t2.child_tags
    in
    let f seg n n' = Some (union_node ~prefix:(prefix #< seg) m n n') in
    split_d_t default_tag @@ SegMap.union ~f augmented1 augmented2
  in
  {root; children}, {default_tag; root_tag; child_tags}

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
    let root, root_tag = union_root t1.default_tag (m prefix) (d1.root, get_root_tag t1) (Some (fst v2), snd v2) in
    Some ({d1 with root}, {t1 with root_tag})

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
    mk_tree (None, d.children) {t with root_tag = None}

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
        | d, None -> iter_data_node ~prefix:(prefix #< seg) f (d, t.default_tag)
        | d, Some t -> iter_node ~prefix:(prefix #< seg) f (d, t))
let iter ?(prefix=Emp) f v = Option.fold ~none:() ~some:(iter_node ~prefix f) v

let rec map_data_node ~prefix f (d, dt) =
  { root = Option.map (fun r -> f prefix (r, dt)) d.root
  ; children = SegMap.mapi ~f:(fun seg d -> map_data_node ~prefix:(prefix #< seg) f (d, dt)) d.children
  }
let rec map_node ~prefix f (d, t) =
  { root = Option.map (fun r -> f prefix (r, get_root_tag t)) d.root
  ; children =
      SegMap.mapi (comb_d_t d.children t.child_tags) ~f:(fun seg ->
          function
          | d, None -> map_data_node ~prefix:(prefix #< seg) f (d, t.default_tag)
          | d, Some t -> map_node ~prefix:(prefix #< seg) f (d, t)) }
let map_data ?(prefix=Emp) f v = Option.map (fun (d, t) -> map_node ~prefix f (d, t), t) v

let rec filter_map_data_node ~prefix f (d, dt) =
  let root = Option.bind d.root @@ fun r -> f prefix (r, dt) in
  let children =
    SegMap.filter_map
      ~f:(fun seg d -> filter_map_data_node ~prefix:(prefix #< seg) f (d, dt))
      d.children
  in
  mk_data_tree (root, children)
let rec filter_map_node ~prefix f (d, t) : _ t =
  let root, root_tag =
    match Option.bind d.root (fun r -> f prefix (r, get_root_tag t)) with
    | None -> None, None
    | Some r -> Some r, t.root_tag
  in
  let children, child_tags =
    let def_t = default_tag_node t.default_tag in
    split_d_t t.default_tag @@
    SegMap.filter_map (comb_d_t d.children t.child_tags)
      ~f:(fun seg ->
          function
          | d, None -> Option.map (fun d -> d, def_t) (filter_map_data_node ~prefix:(prefix #< seg) f (d, t.default_tag))
          | d, Some t -> filter_map_node ~prefix:(prefix #< seg) f (d, t))
  in
  mk_tree (root, children) {t with root_tag; child_tags}
let filter_map_data ?(prefix=Emp) f t = Option.bind t @@ filter_map_node ~prefix f

let filter ?prefix f = filter_map_data ?prefix @@
  fun prefix (d, t) -> if f prefix (d, t) then Some d else None

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
  | seg, (d, None) -> data_node_to_seq_with_bwd_paths ~prefix:(prefix #< seg) (d, dt)
  | seg, (d, Some t) -> node_to_seq_with_bwd_paths ~prefix:(prefix #< seg) (d, t)

let to_seq_with_bwd_paths ?(prefix=Emp) t =
  Option.fold ~none:Seq.empty ~some:(node_to_seq_with_bwd_paths ~prefix) t

let to_seq_values t = Seq.map (fun (_, (d, _)) -> d) @@
  to_seq_with_bwd_paths t

let to_seq_values_with_tags t = Seq.map snd @@
  to_seq_with_bwd_paths t

let to_seq ?prefix t = Seq.map (fun (p, v) -> Bwd.to_list p, v) @@
  to_seq_with_bwd_paths ?prefix t

let of_seq_with_merger ?prefix m = Seq.fold_left (union_singleton ?prefix m) empty

let of_seq s = of_seq_with_merger ~prefix:Emp (fun _ _ y -> y) s

(** {1 Tags} *)

type 'data untagged = 'data data_node option

let untag (v : _ t) : _ untagged = Option.map fst v

let tag t : _ untagged -> _ t = Option.map (fun d -> d, default_tag_node t)

let retag t (v : _ t) : _ t = tag t (untag v)

module Untagged =
struct
  type 'data t = 'data untagged

  let to_seq ?prefix v = Seq.map (fun (p, (d, ())) -> p, d) @@ to_seq ?prefix (tag () v)

  let to_seq_with_bwd_paths ?prefix v = Seq.map (fun (p, (d, ())) -> p, d) @@ to_seq_with_bwd_paths ?prefix (tag () v)

  let to_seq_values v = to_seq_values (tag () v)

  let of_seq s = untag @@ of_seq @@ Seq.map (fun (p, d) -> (p, (d, ()))) s
end
