type path = string list

type 'a act =
  | ActCheckExistence of
      { if_existing : [`Keep | `Hide]
      ; if_absent : [`Ok | `Error]
      }
  | ActFilterMap of ('a -> 'a option)

let act_use = ActCheckExistence {if_existing = `Keep; if_absent = `Error}
let act_hide = ActCheckExistence {if_existing = `Hide; if_absent = `Error}
let act_ignore = ActCheckExistence {if_existing = `Hide; if_absent = `Ok}

type 'a pattern =
  | PatAct of 'a act
  | PatSplit of
      { mode : [`Subtree | `Node]
      ; prefix : path
      ; prefix_replacement : path option
      ; on_target : 'a pattern
      ; on_others : 'a pattern
      }
  | PatSeq of 'a pattern list
  | PatUnion of 'a pattern list

let id = PatSeq []
let hide = PatAct act_hide
let use = PatAct act_use
let ignore = PatAct act_ignore

let any = use
let none = hide

let only_subtree prefix on_target =
  PatSplit {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = ignore}
let only_singleton prefix on_target =
  PatSplit {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = ignore}

let wildcard =
  PatSplit {mode = `Node; prefix = []; prefix_replacement = None; on_target = ignore; on_others = use}
let root = PatSplit {mode = `Node; prefix = []; prefix_replacement = None; on_target = use; on_others = ignore}
let only p = only_singleton p use
let prefix p = only_subtree p use

let update_subtree prefix on_target =
  PatSplit {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = id}
let update_singleton prefix on_target =
  PatSplit {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = id}

let except_root =
  PatSplit {mode = `Node; prefix = []; prefix_replacement = None; on_target = hide; on_others = id}
let except p = update_singleton p hide
let except_prefix p = update_subtree p hide

let renaming_subtree prefix prefix_replacement on_target =
  PatSplit {mode = `Subtree; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}
let renaming_singleton prefix prefix_replacement on_target =
  PatSplit {mode = `Node; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}

let renaming p p' = renaming_singleton p p' use
let renaming_prefix p p' = renaming_subtree p p' use

let seq pats = PatSeq pats

let filter_map f = PatAct (ActFilterMap f)

let union l = PatUnion l
