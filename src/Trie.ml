open StdLabels
open MoreLabels

type seg = string
type path = seg list

module SegMap =
struct
  include Map.Make (struct
      type t = seg
      let compare = String.compare
    end)

  let filter_mapi f m =
    let f ~key ~data m =
      match f ~key data with
      | None -> m
      | Some data -> add ~key ~data m
    in
    fold ~f m ~init:empty

  let filter_mapi_endo f m =
    let f m (key, children) =
      update ~key ~f:(fun _ -> f ~key children) m
    in
    Seq.fold_left f m (to_seq m)
end

type 'a node = {
  root : 'a option;
  children : 'a node SegMap.t;
}

type 'a t = 'a node option

let empty : 'a t = None

let is_empty : 'a t -> bool = Option.is_none

let[@inline] non_empty (t : 'a node) : 'a t = Some t

(** {1 Making (non-empty) trees} *)

let[@inline] is_empty_ root children = Option.is_none root && SegMap.is_empty children

let mk_tree root children =
  if is_empty_ root children then empty else non_empty {root; children}

let[@inline] root_opt_node v = {root = Some v; children = SegMap.empty}

let root_opt v = Option.map root_opt_node v

let prefix_node path n : 'a node =
  let f seg n =
    {root = None; children = SegMap.singleton seg n}
  in
  List.fold_right ~f path ~init:n

let[@inline] prefix path = Option.map @@ prefix_node path

let[@inline] singleton_node (path, v) = prefix_node path @@ root_opt_node v

let[@inline] singleton (path, v) = non_empty @@ singleton_node (path, v)

let[@inline] root v = non_empty @@ root_opt_node v

(** {1 Comparison} *)

let rec equal_node eq n1 n2 =
  n1 == n2 || Option.equal eq n1.root n2.root && SegMap.equal ~cmp:(equal_node eq) n1.children n2.children

let equal eq = Option.equal (equal_node eq)

(** {1 Getting data} *)

let rec find_node_cont path n k =
  match path with
  | [] -> k n
  | seg::path ->
    Option.bind (SegMap.find_opt seg n.children) @@ fun t ->
    find_node_cont path t k

let find_subtree path t =
  Option.bind t @@ fun n -> find_node_cont path n non_empty

let find_singleton path t =
  Option.bind t @@ fun n -> find_node_cont path n @@ fun n -> n.root

let find_root t = find_singleton [] t

(** {1 Traversing the trees} *)

let rec update_node_cont n path (k : 'a t -> 'a t) =
  match path with
  | [] -> k @@ non_empty n
  | seg::path ->
    mk_tree n.root @@
    SegMap.update ~key:seg ~f:(fun n -> update_cont n path k) n.children

and update_cont t path k =
  match t with
  | None -> prefix path @@ k empty
  | Some n -> update_node_cont n path k

let update_subtree path f t = update_cont t path f

let update_singleton path f t = update_cont t path @@
  function
  | None -> root_opt @@ f None
  | Some n -> mk_tree (f n.root) n.children

let update_root f t = update_singleton [] f t

(** {1 Union} *)

let union_option f x y =
  match x, y with
  | _, None -> x
  | None, _ -> y
  | Some x', Some y' -> Some (f x' y')

let rec union_node ~rev_prefix m n n' =
  let root = union_option (m ~rev_path:rev_prefix) n.root n'.root in
  let children =
    if SegMap.is_empty n.children then
      n'.children
    else if SegMap.is_empty n'.children then
      n.children
    else
      let f key n n' = Some (union_node ~rev_prefix:(key :: rev_prefix) m n n') in
      SegMap.union ~f n.children n'.children
  in
  {root; children}

let union_ ~rev_prefix m = union_option @@ union_node ~rev_prefix m

let[@inline] union ?(rev_prefix=[]) m = union_ ~rev_prefix m

let union_subtree ?(rev_prefix=[]) m t (path, t') =
  update_cont t path @@ fun t -> union_ ~rev_prefix:(List.rev_append path rev_prefix) m t t'

let union_singleton ?(rev_prefix=[]) m t (path, v) =
  update_cont t path @@ function
  | None -> non_empty @@ root_opt_node v
  | Some n -> non_empty {n with root = union_option (m ~rev_path:(List.rev_append path rev_prefix)) n.root @@ Some v}

(** {1 Detaching subtrees} *)

let rec apply_and_update_node_cont path n k =
  match path with
  | [] -> k @@ non_empty n
  | seg::path ->
    let ans, new_child = apply_and_update_cont path (SegMap.find_opt seg n.children) k in
    let children = SegMap.update ~key:seg ~f:(Fun.const new_child) n.children in
    ans, mk_tree n.root children

and apply_and_update_cont path t (k : 'a t -> 'b * 'a t) : 'b * 'a t =
  match t with
  | None -> let ans, t = k empty in ans, prefix path t
  | Some n -> apply_and_update_node_cont path n k

let detach_subtree path t = apply_and_update_cont path t @@ fun t -> t, empty

let detach_singleton path t = apply_and_update_cont path t @@ function
  | None -> None, empty
  | Some n -> n.root, mk_tree None n.children

(** {1 Conversion from/to Seq} *)

let rec node_to_seq_with_reversed_paths ~rev_prefix n () =
  match n.root with
  | None -> children_to_seq_with_reversed_paths ~rev_prefix n.children ()
  | Some v ->
    Seq.Cons ((rev_prefix, v), children_to_seq_with_reversed_paths ~rev_prefix n.children)

and children_to_seq_with_reversed_paths ~rev_prefix children =
  SegMap.to_seq children |> Seq.flat_map @@ fun (seg, n) ->
  node_to_seq_with_reversed_paths ~rev_prefix:(seg :: rev_prefix) n

let to_seq_with_reversed_paths ?(rev_prefix=[]) t =
  Option.fold ~none:Seq.empty ~some:(node_to_seq_with_reversed_paths ~rev_prefix) t

let to_seq_values t = Seq.map snd @@
  to_seq_with_reversed_paths t

let to_seq ?rev_prefix t = Seq.map (fun (p, v) -> List.rev p, v) @@
  to_seq_with_reversed_paths ?rev_prefix t

let of_seq ?rev_prefix m = Seq.fold_left (union_singleton ?rev_prefix m) empty

(** {1 Map} *)

let rec iteri_node ~rev_prefix f n =
  Option.fold ~none:() ~some:(f ~rev_path:rev_prefix) n.root;
  SegMap.iter ~f:(fun ~key ~data -> iteri_node ~rev_prefix:(key::rev_prefix) f data) n.children
let iteri ?(rev_prefix=[]) f t = Option.fold ~none:() ~some:(iteri_node ~rev_prefix f) t

let rec mapi_node ~rev_prefix f n =
  { root = Option.map (f ~rev_path:rev_prefix) n.root
  ; children = SegMap.mapi ~f:(fun key -> mapi_node ~rev_prefix:(key::rev_prefix) f) n.children
  }
let mapi ?(rev_prefix=[]) f t = Option.map (mapi_node ~rev_prefix f) t

let rec filteri_node ~rev_prefix f n =
  let root = Option.bind n.root @@
    fun v -> if f ~rev_path:rev_prefix v then Some v else None in
  let children =
    SegMap.filter_mapi_endo
      (fun ~key -> filteri_node ~rev_prefix:(key::rev_prefix) f)
      n.children
  in
  mk_tree root children
let filteri ?(rev_prefix=[]) f t = Option.bind t @@ filteri_node ~rev_prefix f

let rec filter_mapi_node ~rev_prefix f n =
  mk_tree (Option.bind n.root @@ f ~rev_path:rev_prefix) @@
  SegMap.filter_mapi (fun ~key -> filter_mapi_node ~rev_prefix:(key::rev_prefix) f) n.children
let filter_mapi ?(rev_prefix=[]) f t = Option.bind t @@ filter_mapi_node ~rev_prefix f

module Result =
struct
  open ResultMonad.Syntax

  (** {1 Traversing the trees, with result} *)

  let rec update_node_cont n path (k : 'a t -> ('a t, 'b) result) =
    match path with
    | [] -> k @@ non_empty n
    | seg::path ->
      let+ t = update_cont (SegMap.find_opt seg n.children) path k in
      mk_tree n.root @@ SegMap.update ~key:seg ~f:(fun _ -> t) n.children

  and update_cont t path k =
    match t with
    | None -> let+ t = k empty in prefix path t
    | Some n -> update_node_cont n path k

  let update_subtree path f t = update_cont t path f

  let update_singleton path f t = update_cont t path @@
    function
    | None -> let+ t = f None in root_opt t
    | Some n -> let+ r = f n.root in mk_tree r n.children

  let update_root f t = update_singleton [] f t
end
