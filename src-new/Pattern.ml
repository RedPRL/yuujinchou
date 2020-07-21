type path = string list

type 'a act =
  | ActExists of
      { if_existing : [`Keep | `Hide]
      ; if_absent : [`Ok | `Error]
      }
  | ActFilterMap of ('a -> 'a option)

let use = ActExists {if_existing = `Keep; if_absent = `Error}
let hide = ActExists {if_existing = `Hide; if_absent = `Error}
let ignore = ActExists {if_existing = `Hide; if_absent = `Ok}

type 'a pattern =
  | PatAct of 'a act
  | PatRootSplit of
      { on_root : 'a pattern
      ; on_children : 'a pattern
      }
  | PatScopeSplit of
      { prefix : path
      ; prefix_replacement : path option
      ; on_subtree : 'a pattern
      ; on_others : 'a pattern
      }
  | PatSeq of 'a pattern list
  | PatUnion of 'a pattern list

let id = PatSeq []
let none = PatAct hide
let any = PatAct use

let wildcard = PatRootSplit {on_root = PatAct ignore; on_children = any}
let root = PatRootSplit {on_root = any; on_children = PatAct ignore}
let only_scope prefix on_subtree =
  PatScopeSplit {prefix; prefix_replacement = None; on_subtree; on_others = PatAct ignore}
let only x = only_scope x root
let prefix x = only_scope x any

let update_scope prefix on_subtree =
  PatScopeSplit {prefix; prefix_replacement = None; on_subtree; on_others = id}
let except_root = PatRootSplit {on_root = PatAct hide; on_children = id}
let except x = update_scope x except_root
let except_prefix x = update_scope x none

let renaming_scope prefix prefix_replacement on_subtree =
  PatScopeSplit {prefix; prefix_replacement = Some prefix_replacement; on_subtree; on_others = id}
let renaming_prefix x x' = renaming_scope x x' any

let seq pats = PatSeq pats

let filter_map f = PatAct (ActFilterMap f)

let union l = PatUnion l
