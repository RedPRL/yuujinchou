type path = string list

type switch = [`Keep | `Hide]

type 'a act =
  | A_switch of switch
  | A_filter_map of ('a -> 'a option)

let pp_act _pp_data fmt =
  function
  | A_switch `Keep -> Format.pp_print_string fmt "use"
  | A_switch `Hide -> Format.pp_print_string fmt "hide"
  | A_filter_map _ -> Format.pp_print_string fmt "@[<hov 1>(map-filter@ <fun>)@]"

let act_use = A_switch `Keep
let act_hide = A_switch `Hide

type 'a t =
  | P_act of 'a act
  | P_split of
      { mode : [`Subtree | `Node]
      ; prefix : path
      ; prefix_replacement : path option
      ; on_target : 'a t
      ; on_others : 'a t
      }
  | P_seq of 'a t list
  | P_union of 'a t list

let id = P_seq []
let hide = P_act act_hide
let use = P_act act_use
let ignore = P_union []

let any = use
let none = hide

let select_and_update_subtree prefix on_target =
  P_split {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = ignore}
let select_and_update_singleton prefix on_target =
  P_split {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = ignore}

let wildcard =
  P_split {mode = `Node; prefix = []; prefix_replacement = None; on_target = ignore; on_others = use}
let root = P_split {mode = `Node; prefix = []; prefix_replacement = None; on_target = use; on_others = ignore}
let only p = select_and_update_singleton p use
let only_subtree p = select_and_update_subtree p use

let update_subtree prefix on_target =
  P_split {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = id}
let update_singleton prefix on_target =
  P_split {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = id}

let except p = update_singleton p hide
let except_subtree p = update_subtree p hide
let in_subtree prefix = update_subtree prefix

let rename_and_update_subtree prefix prefix_replacement on_target =
  P_split {mode = `Subtree; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}
let rename_and_update_singleton prefix prefix_replacement on_target =
  P_split {mode = `Node; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}

let renaming p p' = rename_and_update_singleton p p' use
let renaming_subtree p p' = rename_and_update_subtree p p' use

let seq pats = P_seq pats

let filter_map f = P_act (A_filter_map f)

let union l = P_union l

let equal_act _equal_a a1 a2 =
  match a1, a2 with
  | A_switch s1, A_switch s2 -> s1 = s2
  | A_filter_map _, A_filter_map _ -> failwith "functions not comparable"
  | _ -> false

let rec equal equal_a p1 p2 =
  match p1, p2 with
  | P_act a1, P_act a2 -> equal_act equal_a a1 a2
  | P_split {mode = m1; prefix = p1; prefix_replacement = r1; on_target = t1; on_others = o1},
    P_split {mode = m2; prefix = p2; prefix_replacement = r2; on_target = t2; on_others = o2} ->
    m1 = m2 && p1 = p2 && r1 = r2 && equal equal_a t1 t2 && equal equal_a o1 o2
  | P_seq ps1, P_seq ps2 | P_union ps1, P_union ps2 ->
    begin try List.for_all2 (equal equal_a) ps1 ps2 with Invalid_argument _ -> false end
  | _ -> false

let pp_path fmt path =
  Format.pp_print_string fmt @@ String.concat "." path

let pp_split_mode fmt =
  function
  | `Subtree -> Format.pp_print_string fmt "'subtree"
  | `Node -> Format.pp_print_string fmt "'node"

let pp_prefix fmt =
  function
  | (prefix, None) -> pp_path fmt prefix
  | (prefix, Some replacement) ->
    Format.fprintf fmt "@[<hov 1>(=>@ %a@ %a)@]" pp_path prefix pp_path replacement

let rec pp_patterns pp_data fmt =
  function
  | [] -> ()
  | p::ps -> Format.fprintf fmt "@ %a%a" (pp pp_data) p (pp_patterns pp_data) ps

and pp pp_data fmt =
  function
  | P_act act -> Format.fprintf fmt "@[<hov 1>(act@ %a)@]" (pp_act pp_data) act
  | P_split {mode; prefix; prefix_replacement; on_target; on_others} ->
    Format.fprintf fmt "@[<hov 1>(split@ %a@ %a@ %a %a)@]"
      pp_split_mode mode
      pp_prefix (prefix, prefix_replacement)
      (pp pp_data) on_target
      (pp pp_data) on_others
  | P_seq ps ->
    Format.fprintf fmt "@[<hov 1>(seq%a)@]" (pp_patterns pp_data) ps
  | P_union ps ->
    Format.fprintf fmt "@[<hov 1>(union%a)@]" (pp_patterns pp_data) ps
