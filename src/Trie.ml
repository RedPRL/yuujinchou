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
  tag_root : 'a option;
  tag_default_child : 'a option;
  tag_children : 'a tag_node SegMap.t
}

type ('data, 'tag) node = 'data data_node * 'tag tag_node
(*
  Invariants:
  1. t.tag_children must be a subset of d.children
  2. t.tag_root must be used by d.root
  3. t.tag_default_child must be used by some d.children

  Non-invariants:
  1. The tag trie need not be minimum.
  2. This implementation prefers removing tag_default_child.
*)

type ('data, 'tag) t = ('data, 'tag) node option

let empty : _ t = None

let is_empty : _ t -> bool = Option.is_none

let[@inline] non_empty (n : _ node) : _ t = Some n

(** {1 Making (non-empty) trees} *)

(* invariants: the input tag_children must already be a subset of d.children (invariant 1) *)
let mk_tag_node d (tag_root, (tag_default_child, tag_children)) : _ tag_node =
  let tag_root = match d.root with None -> None | _ -> tag_root in
  let tag_default_child =
    if Int.equal (SegMap.cardinal d.children) (SegMap.cardinal tag_children)
    then None
    else tag_default_child
  in
  { tag_root; tag_default_child; tag_children }
let mk_tag_node' d t : _ tag_node = mk_tag_node d (t, (t, SegMap.empty))
let mk_node d tag_params : _ node = d, mk_tag_node d tag_params
let mk_node' d tag : _ node = d, mk_tag_node' d tag

(* invariants: input tag tree must be a subset (if default tags were ignored) *)
let mk_tree (root, children) tag_params : _ t =
  if Option.is_none root && SegMap.is_empty children
  then empty
  else non_empty @@ mk_node {root; children} tag_params

let[@inline] root_node (data, tag) =
  {root = Some data; children = SegMap.empty},
  {tag_root = Some tag; tag_default_child = None; tag_children = SegMap.empty}

let[@inline] root_opt v = Option.map root_node v

let[@inline] root v = non_empty @@ root_node v

let[@inline] prefix_node path n : _ node =
  let f seg (d, t) =
    {root = None; children = SegMap.singleton seg d},
    {tag_root = None; tag_default_child = None; tag_children = SegMap.singleton seg t}
  in
  List.fold_right f path n

let[@inline] prefix path = Option.map @@ prefix_node path

let[@inline] singleton (path, (d, t)) = prefix path (root (d, t))

(** {1 Equality} *)

let get_children_node (d, t) =
  SegMap.merge
    (fun _ d' t' ->
       match d', t' with
       | None, _ -> None
       | Some d, None -> Some (d, mk_tag_node' d t.tag_default_child)
       | Some d, Some t -> Some (d, t))
    d.children t.tag_children
let get_children_node2 (d, t1, t2) =
  SegMap.merge
    (fun _ d_t1 t2' ->
       match d_t1, t2' with
       | None, _ -> None
       | Some (d, t1), None -> Some (d, t1, mk_tag_node' d t2.tag_default_child)
       | Some (d, t1), Some t2 -> Some (d, t1, t2))
    (get_children_node (d, t1)) t2.tag_children
let split_children combined = SegMap.map fst combined, SegMap.map snd combined
let split_option = function None -> None, None | Some (d, t) -> Some d, Some t

let rec equal_tag_node eq_tag (d, t1, t2) =
  t1 == t2 ||
  Option.equal eq_tag t1.tag_root t2.tag_root &&
  equal_tag_children eq_tag (d, t1, t2)

and equal_tag_children eq_tag (d, t1, t2) =
  (Option.equal eq_tag t1.tag_default_child t2.tag_default_child &&
   SegMap.is_empty t1.tag_children && SegMap.is_empty t2.tag_children) ||
  SegMap.for_all (fun _ -> equal_tag_node eq_tag) (get_children_node2 (d, t1, t2))

let rec equal_data_node eq n1 n2 =
  n1 == n2 ||
  Option.equal eq n1.root n2.root &&
  SegMap.equal (equal_data_node eq) n1.children n2.children

let equal_node eq_data eq_tag (d1, t1) (d2, t2) =
  (d1 == d2 || equal_data_node eq_data d1 d2) &&
  (t1 == t2 || equal_tag_node eq_tag (d1, t1, t2))

let equal eq_data eq_tag = Option.equal (equal_node eq_data eq_tag)

(** {1 Getting data} *)

let find_child_node seg (d, t) : _ node option =
  match SegMap.find_opt seg d.children with
  | None -> None
  | Some d ->
    match SegMap.find_opt seg t.tag_children with
    | Some t -> Some (d, t)
    | None -> Some (mk_node' d t.tag_default_child)

let rec find_node_cont path n k =
  match path with
  | [] -> k n
  | seg::path ->
    Option.bind (find_child_node seg n) @@ fun n ->
    find_node_cont path n k

let find_subtree path v =
  Option.bind v @@ fun n -> find_node_cont path n non_empty

let find_root_node (d, t) =
  match d.root with
  | None -> None
  | Some r -> Some (r, Option.get t.tag_root)

let find_singleton path v =
  Option.bind v @@ fun n -> find_node_cont path n find_root_node

let find_root v = Option.bind v find_root_node

(** {1 Updating} *)

let rec update_node_cont path (d, t) (k : (_, 'tag) t -> (_, 'tag) t) =
  match path with
  | [] -> k @@ non_empty (d, t)
  | seg::path ->
    let child, tag_child =
      split_option @@
      match find_child_node seg (d, t) with
      | None -> prefix path @@ k empty
      | Some n -> update_node_cont path n k
    in
    let children = SegMap.update seg (fun _ -> child) d.children
    and tag_children = SegMap.update seg (fun _ -> tag_child) t.tag_children
    in
    mk_tree (d.root, children) (t.tag_root, (t.tag_default_child, tag_children))

let update_cont path v k =
  match v with
  | None -> prefix path @@ k empty
  | Some n -> update_node_cont path n k

let update_subtree path f v = update_cont path v f

let update_root f =
  function
  | None -> root_opt @@ f None
  | Some (d, t) ->
    let root, tag_root = split_option @@ f (find_root_node (d, t)) in
    mk_tree (root, d.children) (tag_root, (t.tag_default_child, t.tag_children))

let update_singleton path f v = update_cont path v (update_root f)

(** {1 Union} *)

let union_option m r1 r2 =
  match r1, r2 with
  | None, None -> None
  | Some r, None | None, Some r -> Some r
  | Some r1, Some r2 -> Some (m r1 r2)

let rec union_node ~prefix m n1 n2 =
  let root, tag_root = split_option @@
    union_option (m prefix) (find_root_node n1) (find_root_node n2)
  in
  let children, tag_children =
    split_children @@ SegMap.union
      (fun seg n1 n2 -> Some (union_node ~prefix:(prefix #< seg) m n1 n2))
      (get_children_node n1) (get_children_node n2)
  in
  {root; children}, {tag_root; tag_default_child = None; tag_children}

let union_ ~prefix m = union_option (union_node ~prefix m)

let[@inline] union ?(prefix=Emp) m = union_ ~prefix m

let union_subtree ?(prefix=Emp) m v1 (path, v2) =
  update_cont path v1 @@ fun v1 -> union_ ~prefix:(prefix <>< path) m v1 v2

let union_root ?(prefix=Emp) m v1 v2 =
  match v1 with
  | None -> root v2
  | Some (d1, t1) ->
    let root, tag_root = split_option @@
      union_option (m prefix) (find_root_node (d1, t1)) (Some v2)
    in
    non_empty ({d1 with root}, {t1 with tag_root})

let union_singleton ?(prefix=Emp) m v1 (path, v2) =
  update_cont path v1 @@ fun v1 -> union_root ~prefix:(prefix <>< path) m v1 v2

(** {1 Detaching subtrees} *)

let apply_and_update_cont path t (k : _ t -> 'ans * _ t) : 'ans * _ t =
  match t with
  | None -> let ans, t = k empty in ans, prefix path t
  | Some n ->
    let ans = ref None in
    let t = update_node_cont path n (fun t -> let a, t = k t in ans := Some a; t) in
    Option.get !ans, t

let detach_subtree path t = apply_and_update_cont path t @@ fun t -> t, empty

let detach_root =
  function
  | None -> None, empty
  | Some (d, t) ->
    find_root_node (d, t),
    mk_tree (None, d.children) (None, (t.tag_default_child, t.tag_children))

let detach_singleton path t = apply_and_update_cont path t detach_root

(** {1 Iteration} *)

let rec iter_node ~prefix f n =
  Option.iter (f prefix) (find_root_node n);
  SegMap.iter (fun seg -> iter_node ~prefix:(prefix #< seg) f) (get_children_node n)
let iter ?(prefix=Emp) f v = Option.fold ~none:() ~some:(iter_node ~prefix f) v

let rec filter_map_node ~prefix f n : _ t =
  let root, tag_root = split_option @@ Option.bind (find_root_node n) (f prefix) in
  let children, tag_children =
    split_children @@
    SegMap.filter_map (fun seg -> filter_map_node ~prefix:(prefix #< seg) f) (get_children_node n)
  in
  mk_tree (root, children) (tag_root, (None, tag_children))
let filter_map ?(prefix=Emp) f v = Option.bind v @@ filter_map_node ~prefix f

let map ?prefix f = filter_map ?prefix @@ fun prefix (d, t) -> Some (f prefix (d, t))

let filter ?prefix f = filter_map ?prefix @@
  fun prefix (d, t) -> if f prefix (d, t) then Some (d, t) else None

(** {1 Conversion from/to Seq} *)

let rec node_to_seq_with_bwd_paths ~prefix n () =
  let kont () =
    children_to_seq_with_bwd_paths ~prefix n ()
  in
  match find_root_node n with
  | None -> kont ()
  | Some r -> Seq.Cons ((prefix, r), kont)
and children_to_seq_with_bwd_paths ~prefix n =
  Seq.flat_map
    (fun (seg, n) -> node_to_seq_with_bwd_paths ~prefix:(prefix #< seg) n)
    (SegMap.to_seq (get_children_node n))

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
  | Some (d, _) -> non_empty @@ mk_node' d (Some t)

let retag_subtree path t (v : _ t) : _ t = update_subtree path (retag t) v
