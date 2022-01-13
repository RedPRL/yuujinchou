open StdLabels

type path = string list

let pp_path fmt path =
  Format.pp_print_string fmt @@ String.concat ~sep:"." path

type switch = [`Use | `Hide]

type 'hooks act =
  | A_switch of switch
  | A_hook of 'hooks

let act_use = A_switch `Use
let act_hide = A_switch `Hide

type 'hook split =
  { prefix : path
  ; prefix_replacement : path option
  ; on_target : 'hook t
  ; on_others : 'hook t
  }

and 'hook t =
  | P_act of 'hook act
  | P_split of 'hook split
  | P_seq of 'hook t list
  | P_union of 'hook t list

let id = P_seq []
let hide = P_act act_hide
let use = P_act act_use
let ignore = P_union []

let any = use
let none = hide

let[@inline] select_and_update prefix on_target =
  P_split {prefix; prefix_replacement = None; on_target; on_others = ignore}

let only p = select_and_update p use

let update prefix on_target =
  P_split {prefix; prefix_replacement = None; on_target; on_others = id}

let except p = update p hide
let in_ prefix = update prefix

let rename_and_update prefix prefix_replacement on_target =
  P_split {prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}

let renaming p p' = rename_and_update p p' use

let seq pats = P_seq pats

let hook f = P_act (A_hook f)

let union l = P_union l

let equal_act equal_hook a1 a2 =
  match a1, a2 with
  | A_switch s1, A_switch s2 -> s1 = s2
  | A_hook f1, A_hook f2 -> equal_hook f1 f2
  | _ -> false

let rec equal equal_hook p1 p2 =
  match p1, p2 with
  | P_act a1, P_act a2 -> equal_act equal_hook a1 a2
  | P_split s1, P_split s2 ->
    s1.prefix = s2.prefix &&
    s1.prefix_replacement = s2.prefix_replacement &&
    equal equal_hook s1.on_target s2.on_target &&
    equal equal_hook s1.on_others s2.on_others
  | P_seq ps1, P_seq ps2 | P_union ps1, P_union ps2 ->
    begin try List.for_all2 ~f:(equal equal_hook) ps1 ps2 with Invalid_argument _ -> false end
  | _ -> false
