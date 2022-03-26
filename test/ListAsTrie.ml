open StdLabels

type seg = string
type path = seg list

(* invariant: items are sorted and uniquefied *)
type +'a t = (path * 'a) list

let empty = []
let is_empty l = l = []

let root x = [[], x]
let root_opt x = Option.fold ~none:[] ~some:root x
let prefix pre l = List.map l ~f:(fun (p, x) -> pre @ p, x)
let singleton x = [x]

let (=) = List.equal ~eq:String.equal
let equal eq l1 l2 =
  List.equal ~eq:(fun (p1, x1) (p2, x2) -> p1 = p2 && eq x1 x2) l1 l2

let rec split_path pre p =
  match pre, p with
  | [], _ -> Some p
  | _, [] -> None
  | x::pre, y::p -> if String.equal x y then split_path pre p else None

let find_subtree pre l =
  List.filter_map l ~f:(fun (p, x) -> Option.map (fun suf -> suf, x) @@ split_path pre p)
let find_singleton p l =
  Option.map snd @@ List.find_opt l ~f:(fun (p', _) -> p = p')
let find_root l = find_singleton [] l

let iteri ?(rev_prefix=[]) f l =
  List.iter l ~f:(fun (p, x) -> f ~rev_path:(List.rev_append p rev_prefix) x)
let mapi ?(rev_prefix=[]) f l =
  List.map l ~f:(fun (p, x) -> (p, f ~rev_path:(List.rev_append p rev_prefix) x))
let filteri ?(rev_prefix=[]) f l =
  List.filter l ~f:(fun (p, x) -> f ~rev_path:(List.rev_append p rev_prefix) x)
let filter_mapi ?(rev_prefix=[]) f l =
  List.filter_map l
    ~f:(fun (p, x) ->
        Option.map (fun x -> p, x) @@ f ~rev_path:(List.rev_append p rev_prefix) x)

let detach_subtree pre l =
  List.partition_map l
    ~f:(fun (p, x) ->
        match split_path pre p with
        | Some suf -> Either.Left (suf, x)
        | None -> Either.Right (p, x))
let detach_singleton p l =
  let l1, l2 =
    List.partition_map l
      ~f:(fun b -> if fst b = p then Either.Left b else Either.Right b)
  in
  Option.map snd (List.nth_opt l1 0), l2


let cmp_path = List.compare ~cmp:String.compare
let cmp (p1, _) (p2, _) = cmp_path p1 p2

let rec uniquefy_sorted ~rev_prefix ~union =
  function
  | [] -> []
  | [b] -> [b]
  | (p1,x1)::(p2,x2)::rest ->
    if p1 = p2 then
      let merged = (p1, union ~rev_path:(List.rev_append p1 rev_prefix) x1 x2) in
      uniquefy_sorted ~rev_prefix ~union @@ merged::rest
    else
      (p1,x1)::(uniquefy_sorted ~rev_prefix ~union @@ (p2,x2)::rest)

let union ?(rev_prefix=[]) u l1 l2 =
  uniquefy_sorted ~rev_prefix ~union:u @@ List.stable_sort ~cmp @@ l1 @ l2
let union_subtree ?rev_prefix u l1 (pre, l2) =
  union ?rev_prefix u l1 @@ prefix pre l2
let union_singleton ?rev_prefix u l1 (p, x) =
  union ?rev_prefix u l1 @@ singleton (p, x)

let update_subtree p f l =
  let sub, rest = detach_subtree p l in
  List.sort_uniq ~cmp @@ rest @ (prefix p @@ f sub)
let update_singleton p f l =
  let x, rest = detach_singleton p l in
  List.sort_uniq ~cmp @@ rest @ (prefix p @@ root_opt (f x))
let update_root f l =
  update_singleton [] f l

let to_seq ?(rev_prefix=[]) l =
  Seq.map (fun (p, x) -> List.rev_append rev_prefix p, x) @@ List.to_seq l
let to_seq_with_reversed_paths ?(rev_prefix=[]) l =
  Seq.map (fun (p, x) -> List.rev_append p rev_prefix, x) @@ List.to_seq l
let to_seq_values l = Seq.map snd @@ List.to_seq l
let of_seq ?(rev_prefix=[]) u s = Seq.fold_left (union_singleton ~rev_prefix u) empty s
