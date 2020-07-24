type path = string list

type switch = [`Keep | `Hide]

type 'a act =
  | ActSwitch of switch
  | ActFilterMap of ('a -> 'a option)

let pp_act _pp_data fmt =
  function
  | ActSwitch `Keep -> Format.pp_print_string fmt "use"
  | ActSwitch `Hide -> Format.pp_print_string fmt "hide"
  | ActFilterMap _ -> Format.pp_print_string fmt "@[<hov 1>(map-filter@ <fun>)@]"

let act_use = ActSwitch `Keep
let act_hide = ActSwitch `Hide

type 'a t =
  | PatAct of 'a act
  | PatSplit of
      { mode : [`Subtree | `Node]
      ; prefix : path
      ; prefix_replacement : path option
      ; on_target : 'a t
      ; on_others : 'a t
      }
  | PatSeq of 'a t list
  | PatUnion of 'a t list

let id = PatSeq []
let hide = PatAct act_hide
let use = PatAct act_use
let ignore = PatUnion []

let any = use
let none = hide

let select_and_update_subtree prefix on_target =
  PatSplit {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = ignore}
let select_and_update_singleton prefix on_target =
  PatSplit {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = ignore}

let wildcard =
  PatSplit {mode = `Node; prefix = []; prefix_replacement = None; on_target = ignore; on_others = use}
let root = PatSplit {mode = `Node; prefix = []; prefix_replacement = None; on_target = use; on_others = ignore}
let only p = select_and_update_singleton p use
let only_subtree p = select_and_update_subtree p use

let update_subtree prefix on_target =
  PatSplit {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = id}
let update_singleton prefix on_target =
  PatSplit {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = id}

let except p = update_singleton p hide
let except_subtree p = update_subtree p hide
let in_subtree prefix = update_subtree prefix

let rename_and_update_subtree prefix prefix_replacement on_target =
  PatSplit {mode = `Subtree; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}
let rename_and_update_singleton prefix prefix_replacement on_target =
  PatSplit {mode = `Node; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}

let renaming p p' = rename_and_update_singleton p p' use
let renaming_subtree p p' = rename_and_update_subtree p p' use

let seq pats = PatSeq pats

let filter_map f = PatAct (ActFilterMap f)

let union l = PatUnion l

let equal_act _equal_a a1 a2 =
  match a1, a2 with
  | ActSwitch s1, ActSwitch s2 -> s1 = s2
  | ActFilterMap _, ActFilterMap _ -> failwith "functions not comparable"
  | _ -> false

let rec equal equal_a p1 p2 =
  match p1, p2 with
  | PatAct a1, PatAct a2 -> equal_act equal_a a1 a2
  | PatSplit {mode = m1; prefix = p1; prefix_replacement = r1; on_target = t1; on_others = o1},
    PatSplit {mode = m2; prefix = p2; prefix_replacement = r2; on_target = t2; on_others = o2} ->
    m1 = m2 && p1 = p2 && r1 = r2 && equal equal_a t1 t2 && equal equal_a o1 o2
  | PatSeq ps1, PatSeq ps2 | PatUnion ps1, PatUnion ps2 ->
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
  | PatAct act -> Format.fprintf fmt "@[<hov 1>(act@ %a)@]" (pp_act pp_data) act
  | PatSplit {mode; prefix; prefix_replacement; on_target; on_others} ->
    Format.fprintf fmt "@[<hov 1>(split@ %a@ %a@ %a %a)@]"
      pp_split_mode mode
      pp_prefix (prefix, prefix_replacement)
      (pp pp_data) on_target
      (pp pp_data) on_others
  | PatSeq ps ->
    Format.fprintf fmt "@[<hov 1>(seq%a)@]" (pp_patterns pp_data) ps
  | PatUnion ps ->
    Format.fprintf fmt "@[<hov 1>(union%a)@]" (pp_patterns pp_data) ps
