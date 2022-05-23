open Bwd
open BwdNotation

type seg = string
type path = seg list
type bwd_path = seg bwd

(* invariant: items are sorted and uniquefied *)
type (+!'a, +!'b) t = (path * ('a * 'b)) list

let empty = []
let is_empty l = l = []

let root x = [[], x]
let root_opt x = Option.fold ~none:[] ~some:root x
let prefix pre l = List.map (fun (p, x) -> pre @ p, x) l
let singleton x = [x]

let (=) = List.equal String.equal
let equal eq_data eq_tag l1 l2 =
  List.equal (fun (p1, (d1, t1)) (p2, (d2, t2)) -> p1 = p2 && eq_data d1 d2 && eq_tag t1 t2) l1 l2

let rec split_path pre p =
  match pre, p with
  | [], _ -> Some p
  | _, [] -> None
  | x::pre, y::p -> if String.equal x y then split_path pre p else None

let find_subtree pre l =
  List.filter_map (fun (p, x) -> Option.map (fun suf -> suf, x) @@ split_path pre p) l
let find_singleton p l =
  Option.map snd @@ List.find_opt (fun (p', _) -> p = p') l
let find_root l = find_singleton [] l

let iter ?(prefix=Emp) f l =
  List.iter (fun (p, x) -> f (prefix <>< p) x) l
let map ?(prefix=Emp) f l =
  List.map (fun (p, (d, t)) -> (p, f (prefix <>< p) (d, t))) l
let filter ?(prefix=Emp) f l =
  List.filter (fun (p, (d, t)) -> f (prefix <>< p) (d, t)) l
let filter_map ?(prefix=Emp) f l =
  List.filter_map (fun (p, (d, t)) -> Option.map (fun x -> p, x) @@ f (prefix <>< p) (d, t)) l

let detach_subtree pre l =
  l |> List.partition_map
    (fun (p, x) ->
       match split_path pre p with
       | Some suf -> Either.Left (suf, x)
       | None -> Either.Right (p, x))

let detach_singleton p l =
  let l1, l2 =
    List.partition_map (fun b -> if fst b = p then Either.Left b else Either.Right b) l
  in
  Option.map snd (List.nth_opt l1 0), l2
let detach_root l =
  let l1, l2 =
    List.partition_map (fun b -> if fst b = [] then Either.Left b else Either.Right b) l
  in
  Option.map snd (List.nth_opt l1 0), l2


let cmp_path = List.compare String.compare
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
  uniquefy_sorted ~prefix ~merger:m @@ List.stable_sort cmp @@ l1 @ l2
let union_subtree ?prefix:p m l1 (pre, l2) =
  union ?prefix:p m l1 @@ prefix pre l2
let union_singleton ?prefix m l1 (p, x) =
  union ?prefix m l1 @@ singleton (p, x)
let union_root ?prefix m l1 x =
  union ?prefix m l1 @@ root x

let update_subtree p f l =
  let sub, rest = detach_subtree p l in
  List.sort_uniq cmp @@ rest @ (prefix p @@ f sub)
let update_singleton p f l =
  let x, rest = detach_singleton p l in
  List.sort_uniq cmp @@ rest @ (prefix p @@ root_opt (f x))
let update_root f l =
  update_singleton [] f l

let to_seq ?(prefix=Emp) l =
  Seq.map (fun (p, x) -> prefix <>> p, x) @@ List.to_seq l
let to_seq_with_bwd_paths ?(prefix=Emp) l =
  Seq.map (fun (p, x) -> prefix <>< p, x) @@ List.to_seq l
let to_seq_values l = Seq.map snd @@ List.to_seq l
let to_seq_data l = Seq.map (fun (_, (d, _)) -> d) @@ List.to_seq l
let to_seq_tags l = Seq.map (fun (_, (_, t)) -> t) @@ List.to_seq l
let of_seq s = Seq.fold_left (union_singleton ~prefix:Emp (fun _ _ y -> y)) empty s
let of_seq_with_merger ?(prefix=Emp) m s = Seq.fold_left (union_singleton ~prefix m) empty s

let retag t l = List.map (fun (p, (d, _)) -> p, (d, t)) l
let retag_subtree pre t l =
  List.map (fun ((p, (d, _)) as b) -> if Option.is_some (split_path pre p) then p, (d, t) else b) l
