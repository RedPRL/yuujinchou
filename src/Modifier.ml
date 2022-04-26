open StdLabels

type path = string list

type 'hook t =
  | P_only of path
  | P_none
  | P_in of path * 'hook t
  | P_renaming of path * path
  | P_seq of 'hook t list
  | P_union of 'hook t list
  | P_hook of 'hook

let any = P_only []
let none = P_none

let in_ p pat = P_in (p, pat)

let only p = P_only p
let except p = in_ p none

let renaming p p' = P_renaming (p, p')

let seq pats = P_seq pats

let hook f = P_hook f

let union l = P_union l

let (=) = List.equal ~eq:String.equal
let rec equal equal_hook pat1 pat2 =
  match pat1, pat2 with
  | P_only p1, P_only p2 -> p1 = p2
  | P_none, P_none -> true
  | P_in (p1, pat1), P_in (p2, pat2) -> p1 = p2 && equal equal_hook pat1 pat2
  | P_renaming (p1, p1'), P_renaming (p2, p2') -> p1 = p2 && p1' = p2'
  | P_seq ps1, P_seq ps2 | P_union ps1, P_union ps2 ->
    begin try List.for_all2 ~f:(equal equal_hook) ps1 ps2 with Invalid_argument _ -> false end
  | P_hook h1, P_hook h2 -> equal_hook h1 h2
  | _ -> false

let dump_path =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.') Format.pp_print_string

let rec dump dump_hook fmt =
  function
  | P_only p ->
    Format.fprintf fmt "@[<hv 1>only[@,@[%a@]]@]" dump_path p
  | P_none ->
    Format.pp_print_string fmt "none"
  | P_in (p, pat) ->
    Format.fprintf fmt "@[<hv 1>in[@,@[%a@];@,@[%a@]]@]" dump_path p (dump dump_hook) pat
  | P_renaming (p1, p2) ->
    Format.fprintf fmt "@[<hv 1>renaming[@,@[%a@];@,@[%a@]]@]" dump_path p1 dump_path p2
  | P_seq pats ->
    Format.fprintf fmt "@[<hv 1>seq[@,%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") (dump dump_hook)) pats
  | P_union pats ->
    Format.fprintf fmt "@[<hv 1>union[@,%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") (dump dump_hook)) pats
  | P_hook h ->
    Format.fprintf fmt "@[<hv 1>hook[@,@[%a@]]@]" dump_hook h
