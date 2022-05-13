open StdLabels

type ('hook, 'kind) t_ =
  | M_only : Trie.path -> ('hook, [< `Modifier | `Selector]) t_
  | M_none : ('hook, [< `Modifier]) t_
  | M_in : Trie.path * ('hook, [< `Modifier] as 'kind) t_ -> ('hook, 'kind) t_
  | M_renaming : Trie.path * Trie.path -> ('hook, [< `Modifier]) t_
  | M_seq : ('hook, [< `Modifier] as 'kind) t_ list -> ('hook, 'kind) t_
  | M_union : ('hook, [< `Modifier | `Selector] as 'kind) t_ list -> ('hook, 'kind) t_
  | M_hook : 'hook -> ('hook, [< `Modifier | `Selector]) t_

type ('hook, 'kind) t = ('hook, [< `Modifier | `Selector] as 'kind) t_
type 'hook modifier = ('hook, [`Modifier]) t
type 'hook selector = ('hook, [`Selector]) t

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
let rec equal : type kind . ('hook -> 'hook -> bool) -> ('hook, kind) t_ -> ('hook, kind) t_ -> bool =
  fun equal_hook m1 m2 ->
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

let rec dump : type kind . (Format.formatter -> 'hook -> unit) -> Format.formatter -> ('hook, kind) t_ -> unit =
  fun dump_hook fmt ->
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
