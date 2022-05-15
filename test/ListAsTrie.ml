open StdLabels
open Bwd
open BwdNotation

type seg = string
type path = seg list
type bwd_path = seg bwd

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

let iter ?(prefix=Emp) f l =
  List.iter l ~f:(fun (p, x) -> f (prefix <>< p) x)
let map ?(prefix=Emp) f l =
  List.map l ~f:(fun (p, x) -> (p, f (prefix <>< p) x))
let filter ?(prefix=Emp) f l =
  List.filter l ~f:(fun (p, x) -> f (prefix <>< p) x)
let filter_map ?(prefix=Emp) f l =
  List.filter_map l
    ~f:(fun (p, x) ->
        Option.map (fun x -> p, x) @@ f (prefix <>< p) x)

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

let rec uniquefy_sorted ~prefix ~merger =
  function
  | [] -> []
  | [b] -> [b]
  | (p1,x1)::(p2,x2)::rest ->
    if p1 = p2 then
      let merged = (p1, merger (prefix <>< p1) x1 x2) in
      uniquefy_sorted ~prefix ~merger @@ merged::rest
    else
      (p1,x1)::(uniquefy_sorted ~prefix ~merger @@ (p2,x2)::rest)

let union ?(prefix=Emp) m l1 l2 =
  uniquefy_sorted ~prefix ~merger:m @@ List.stable_sort ~cmp @@ l1 @ l2
let union_subtree ?prefix:p m l1 (pre, l2) =
  union ?prefix:p m l1 @@ prefix pre l2
let union_singleton ?prefix m l1 (p, x) =
  union ?prefix m l1 @@ singleton (p, x)

let update_subtree p f l =
  let sub, rest = detach_subtree p l in
  List.sort_uniq ~cmp @@ rest @ (prefix p @@ f sub)
let update_singleton p f l =
  let x, rest = detach_singleton p l in
  List.sort_uniq ~cmp @@ rest @ (prefix p @@ root_opt (f x))
let update_root f l =
  update_singleton [] f l

let to_seq ?(prefix=Emp) l =
  Seq.map (fun (p, x) -> prefix <>> p, x) @@ List.to_seq l
let to_seq_with_bwd_paths ?(prefix=Emp) l =
  Seq.map (fun (p, x) -> prefix <>< p, x) @@ List.to_seq l
let to_seq_values l = Seq.map snd @@ List.to_seq l
let of_seq s = Seq.fold_left (union_singleton ~prefix:Emp (fun _ _ y -> y)) empty s
let of_seq_with_merger ?(prefix=Emp) m s = Seq.fold_left (union_singleton ~prefix m) empty s
