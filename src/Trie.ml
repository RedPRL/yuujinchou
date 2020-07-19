open StdLabels

module StringMap = Map.Make (struct
    type t = string
    let compare = String.compare
  end)

type 'a node = {
  root : 'a option;
  children : 'a node StringMap.t;
}

type path = string list

type 'a t = 'a node option

let empty : 'a t = None
let non_empty (t : 'a node) : 'a t = Some t

(** could be empty *)
let mk_tree root children =
  if Option.is_none root && StringMap.is_empty children
  then empty
  else non_empty {root; children}

let root_node data = {root = Some data; children = StringMap.empty}

let root data = non_empty @@ root_node data

let rec prefix_node path t : 'a node =
  let f seg t =
    {root = None; children = StringMap.singleton seg @@ prefix_node path t}
  in
  List.fold_right ~f path ~init:t

let prefix path = Option.map @@ prefix_node path

let singleton_node (path, data) = prefix_node path @@ root_node data

let singleton (path, data) = non_empty @@ singleton_node (path, data)

let rec find_opt_node_cont path t k =
  match path with
  | [] -> k t
  | seg::path ->
    Option.bind (StringMap.find_opt seg t.children) @@ fun t ->
    find_opt_node_cont path t k

let find_opt path t =
  Option.bind t @@ fun t -> find_opt_node_cont path t @@ fun t -> t.root

let find_subtree_opt path t =
  Option.bind t @@ fun t -> find_opt_node_cont path t non_empty

let union_option f x x' =
  match x, x' with
  | _, None -> x
  | None, _ -> x'
  | Some x, Some x' -> Some (f x x')

let rec union_node m t t' =
  let root = union_option m t.root t'.root in
  let children =
    let f _key t t' = Some (union_node m t t') in
    StringMap.union f t.children t'.children
  in
  {root; children}

let union m = union_option @@ union_node m

let rec union_subtree_node_cont t (path, prefix_k, union_k) =
  match path with
  | [] -> union_k t
  | seg::path ->
    let children = t.children |> StringMap.update seg @@ function
      | None -> non_empty @@ prefix_k path
      | Some t -> non_empty @@ union_subtree_node_cont t (path, prefix_k, union_k)
    in
    {t with children}

let union_subtree m t (path, t') =
  match t, t' with
  | None, _ -> prefix path t'
  | _, None -> t
  | Some t, Some t' ->
    let prefix_k path = prefix_node path t' in
    let union_k t = union_node m t t' in
    non_empty @@ union_subtree_node_cont t (path, prefix_k, union_k)

let union_singleton m t (path, data) =
  match t with
  | None -> singleton (path, data)
  | Some t ->
    let prefix_k path = singleton_node (path, data) in
    let union_k t = {t with root = union_option m t.root @@ Some data} in
    non_empty @@ union_subtree_node_cont t (path, prefix_k, union_k)

let intersect_option f x x' =
  match x, x' with
  | _, None | None, _ -> None
  | Some x, Some x' -> f x x'

let rec intersect_node m t t' =
  let root = intersect_option (fun t t' -> Some (m t t')) t.root t'.root in
  let children =
    StringMap.merge (fun _key -> intersect m) t'.children t.children
  in
  mk_tree root children

and intersect m = intersect_option @@ intersect_node m

let rec detach_node path t =
  match path with
  | [] -> empty, Some t
  | seg::path ->
    let left_child, detached = detach path @@ StringMap.find_opt seg t.children in
    let children = StringMap.update seg (fun _ -> left_child) t.children in
    mk_tree t.root children, detached

and detach path = Option.fold ~none:(empty, empty) ~some:(detach_node path)

let rec node_to_seq prefix_stack t () =
  match t.root with
  | None -> children_to_seq prefix_stack t.children ()
  | Some data ->
    let path = List.rev prefix_stack in
    Seq.Cons ((path, data), children_to_seq prefix_stack t.children)

and children_to_seq prefix_stack children =
  StringMap.to_seq children |> Seq.flat_map @@ fun (seg, t) ->
  node_to_seq (seg :: prefix_stack) t

let to_seq t = Option.fold ~none:Seq.empty ~some:(node_to_seq []) t

let of_seq m = Seq.fold_left (union_singleton m) empty
