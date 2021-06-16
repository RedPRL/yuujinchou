open StdLabels
open MoreLabels
open Bwd

type seg = string
type path = seg list

module SegMap =
struct
  include Map.Make (struct
      type t = seg
      let compare = String.compare
    end)

  let filter_map f m =
    let f ~key ~data m =
      match f data with
      | None -> m
      | Some data -> add ~key ~data m
    in
    fold ~f m ~init:empty

  let filter_map_endo f m =
    let f m (key, children) =
      update ~key ~f:(fun _ -> f children) m
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

let[@inline] mk_root_node v = {root = Some v; children = SegMap.empty}

let mk_root v = Option.map mk_root_node v

let prefix_node path n : 'a node =
  let f seg n =
    {root = None; children = SegMap.singleton seg n}
  in
  List.fold_right ~f path ~init:n

let[@inline] prefix path = Option.map @@ prefix_node path

let[@inline] singleton_node (path, v) = prefix_node path @@ mk_root_node v

let[@inline] singleton (path, v) = non_empty @@ singleton_node (path, v)

let[@inline] root v = non_empty @@ mk_root_node v

(** {1 Comparison} *)

let rec equal_node eq n1 n2 =
  n1 == n2 || Option.equal eq n1.root n2.root && SegMap.equal ~cmp:(equal_node eq) n1.children n2.children

let equal eq = Option.equal (equal_node eq)

(*
let rec compare_node cmp n1 n2 =
  if n1 == n2 then 0 else
    match Option.compare cmp n1.root n2.root with
    | 0 -> SegMap.compare ~cmp:(compare_node cmp) n1.children n2.children
    | n -> n

let compare cmp = Option.compare (compare_node cmp)
*)

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

let phy_eq_option r1 r2 =
  match r1, r2 with
  | None, None -> true
  | Some r1, Some r2 -> r1 == r2
  | _ -> false

let phy_eq_map c1 c2 = c1 == c2

let replace_nonempty_root_and_children n root children =
  if phy_eq_option n.root root && phy_eq_map n.children children
  then n else {root; children}

let replace2_nonempty_root_and_children n1 n2 root children =
  if phy_eq_option n1.root root && phy_eq_map n1.children children
  then n1 else replace_nonempty_root_and_children n2 root children

let replace_root_and_children n root children =
  if phy_eq_option n.root root && phy_eq_map n.children children
  then non_empty n else mk_tree root children

let replace_root n root =
  if phy_eq_option n.root root then non_empty n else mk_tree root n.children

let replace_children n children =
  if phy_eq_map n.children children then non_empty n else mk_tree n.root children

let replace_tree t1 t2 =
  if phy_eq_option t1 t2 then t1 else t2

let rec update_node_cont n path (k : 'a t -> 'a t) =
  match path with
  | [] -> k @@ non_empty n
  | seg::path ->
    replace_children n @@
    SegMap.update ~key:seg ~f:(fun n -> update_cont n path k) n.children

and update_cont t path k =
  replace_tree t @@ match t with
  | None -> prefix path @@ k empty
  | Some n -> update_node_cont n path k

let update_subtree path f t = update_cont t path f

let update_singleton path f t = update_cont t path @@
  function
  | None -> mk_root @@ f None
  | Some n -> replace_root n (f n.root)

let update_root f t = update_singleton [] f t

(** {1 Union} *)

let union_option f x y =
  match x, y with
  | _, None -> x
  | None, _ -> y
  | Some x', Some y' ->
    let fxy = f x' y' in
    if fxy == x' then x
    else if fxy == y' then y
    else Some fxy

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
  replace2_nonempty_root_and_children n n' root children

let union_ ~rev_prefix m = union_option @@ union_node ~rev_prefix m

let[@inline] union m = union_ ~rev_prefix:[] m

let union_subtree m t (path, t') =
  update_cont t path @@ fun t -> union_ ~rev_prefix:(List.rev path) m t t'

let union_singleton m t (path, v) =
  update_cont t path @@ function
  | None -> non_empty @@ mk_root_node v
  | Some n -> replace_root n @@ union_option (m ~rev_path:(List.rev path)) n.root @@ Some v

(** {1 Detaching subtrees} *)

(* TODO preserves physical eq *)
let rec apply_and_update_node_cont path n k =
  match path with
  | [] -> k @@ non_empty n
  | seg::path ->
    let ans, new_child = apply_and_update_cont path (SegMap.find_opt seg n.children) k in
    let children = SegMap.update ~key:seg ~f:(Fun.const new_child) n.children in
    ans, replace_children n children

(* TODO preserves physical eq *)
and apply_and_update_cont path t (k : 'a t -> 'b * 'a t) : 'b * 'a t =
  match t with
  | None -> let ans, t = k empty in ans, prefix path t
  | Some n -> apply_and_update_node_cont path n k

(* TODO preserves physical eq *)
let detach_subtree path t = apply_and_update_cont path t @@ fun t -> t, empty

(* TODO preserves physical eq *)
let detach_singleton path t = apply_and_update_cont path t @@ function
  | None -> None, empty
  | Some n -> n.root, replace_root n None

(** {1 Conversion from/to Seq} *)

let rec node_to_seq prefix n () =
  match n.root with
  | None -> children_to_seq prefix n.children ()
  | Some v ->
    Seq.Cons ((prefix >> [], v), children_to_seq prefix n.children)

and children_to_seq prefix_stack children =
  SegMap.to_seq children |> Seq.flat_map @@ fun (seg, n) ->
  node_to_seq (Snoc (prefix_stack, seg)) n

let to_seq t = Option.fold ~none:Seq.empty ~some:(node_to_seq Nil) t

let rec node_to_seq_values n () =
  match n.root with
  | None -> children_to_seq_values n.children ()
  | Some v ->
    Seq.Cons (v, children_to_seq_values n.children)

and children_to_seq_values children =
  SegMap.to_seq children |> Seq.flat_map @@ fun (_, n) -> node_to_seq_values n

let to_seq_values t = Option.fold ~none:Seq.empty ~some:node_to_seq_values t

let of_seq m = Seq.fold_left (union_singleton m) empty

(** {1 Map} *)

let rec map_node f n =
  { root = Option.map f n.root
  ; children = SegMap.map ~f:(map_node f) n.children
  }
let map f t = Option.map (map_node f) t

let rec map_endo_node f n =
  let root = Option.map f n.root in
  let children = SegMap.filter_map_endo (fun n -> Some (map_endo_node f n)) n.children in
  replace_nonempty_root_and_children n root children
let map_endo f t = replace_tree t @@ Option.map (map_endo_node f) t

let rec filter_node f n =
  let root = Option.bind n.root @@ fun v -> if f v then Some v else None in
  let children = SegMap.filter_map_endo (filter_node f) n.children in
  replace_root_and_children n root children
let filter f t = replace_tree t @@ Option.bind t @@ filter_node f

let rec filter_map_node f n =
  mk_tree (Option.bind n.root f) @@ SegMap.filter_map (filter_map_node f) n.children
let filter_map f t = Option.bind t @@ filter_map_node f

let rec filter_map_endo_node f n =
  let root = Option.bind n.root f in
  let children = SegMap.filter_map_endo (filter_map_endo_node f) n.children in
  replace_root_and_children n root children
let filter_map_endo f t = replace_tree t @@ Option.bind t @@ filter_map_endo_node f

let rec pp_node pp_v fmt {root; children} =
  Format.fprintf fmt "@[@[<hv2>{ . =>@ %a@]%a@ @[<hv2>}@]@]"
    Format.(pp_print_option ~none:(fun fmt () -> pp_print_string fmt "") pp_v) root
    (pp_children pp_v) children

and pp_children pp_v fmt =
  let f ~key:seg ~data:n =
    Format.fprintf fmt "@ @[<hv2>; %a =>@ %a@]" Format.pp_print_string seg (pp_node pp_v) n
  in
  SegMap.iter ~f

let pp pp_v = Format.pp_print_option (pp_node pp_v)

let physically_equal : 'a t -> 'a t -> bool = phy_eq_option
