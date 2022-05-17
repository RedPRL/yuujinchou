open StdLabels

type 'hook t =
  | M_only of Trie.path
  | M_none
  | M_in of Trie.path * 'hook t
  | M_renaming of Trie.path * Trie.path
  | M_seq of 'hook t list
  | M_union of 'hook t list
  | M_hook of 'hook

let any = M_only []
let none = M_none

let in_ p m = M_in (p, m)

let only p = M_only p
let except p = in_ p none

let renaming p p' = M_renaming (p, p')

let seq ms = M_seq ms

let hook f = M_hook f

let union l = M_union l

let (=) = List.equal ~eq:String.equal
let rec equal equal_hook m1 m2 =
  match m1, m2 with
  | M_only p1, M_only p2 -> p1 = p2
  | M_none, M_none -> true
  | M_in (p1, m1), M_in (p2, m2) -> p1 = p2 && equal equal_hook m1 m2
  | M_renaming (p1, p1'), M_renaming (p2, p2') -> p1 = p2 && p1' = p2'
  | M_seq ps1, M_seq ps2 ->
    begin try List.for_all2 ~f:(equal equal_hook) ps1 ps2 with Invalid_argument _ -> false end
  | M_union ps1, M_union ps2 ->
    begin try List.for_all2 ~f:(equal equal_hook) ps1 ps2 with Invalid_argument _ -> false end
  | M_hook h1, M_hook h2 -> equal_hook h1 h2
  | _ -> false

let dump_path =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.') Format.pp_print_string

let rec dump dump_hook fmt =
  function
  | M_only p ->
    Format.fprintf fmt "@[<hv 1>only[@,@[%a@]]@]" dump_path p
  | M_none ->
    Format.pp_print_string fmt "none"
  | M_in (p, m) ->
    Format.fprintf fmt "@[<hv 1>in[@,@[%a@];@,@[%a@]]@]" dump_path p (dump dump_hook) m
  | M_renaming (p1, p2) ->
    Format.fprintf fmt "@[<hv 1>renaming[@,@[%a@];@,@[%a@]]@]" dump_path p1 dump_path p2
  | M_seq ms ->
    Format.fprintf fmt "@[<hv 1>seq[@,%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") (dump dump_hook)) ms
  | M_union ms ->
    Format.fprintf fmt "@[<hv 1>union[@,%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,") (dump dump_hook)) ms
  | M_hook h ->
    Format.fprintf fmt "@[<hv 1>hook[@,@[%a@]]@]" dump_hook h
