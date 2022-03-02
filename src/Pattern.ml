open StdLabels

type path = string list

let pp_path fmt path =
  Format.pp_print_string fmt @@ String.concat ~sep:"." path

type 'hook t =
  | P_only of path
  | P_except of path
  | P_in of path * 'hook t
  | P_renaming of path * path
  | P_seq of 'hook t list
  | P_union of 'hook t list
  | P_hook of 'hook

let any = P_only []
let none = P_except []

let only p = P_only p

let except p = P_except p
let in_ p pat = P_in (p, pat)

let renaming p p' = P_renaming (p, p')

let seq pats = P_seq pats

let hook f = P_hook f

let union l = P_union l

let rec equal equal_hook pat1 pat2 =
  match pat1, pat2 with
  | P_only p1, P_only p2 | P_except p1, P_except p2 -> p1 = p2
  | P_renaming (p1, p1'), P_renaming (p2, p2') -> p1 = p2 && p1' = p2'
  | P_in (p1, pat1), P_in (p2, pat2) -> p1 = p2 && equal equal_hook pat1 pat2
  | P_seq ps1, P_seq ps2 | P_union ps1, P_union ps2 ->
    begin try List.for_all2 ~f:(equal equal_hook) ps1 ps2 with Invalid_argument _ -> false end
  | P_hook h1, P_hook h2 -> equal_hook h1 h2
  | _ -> false
