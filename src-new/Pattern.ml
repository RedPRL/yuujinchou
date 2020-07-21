type path = string list

type 'a act =
  | ActOnExistence of
      { if_existing : [`Keep | `Hide]
      ; if_absent : [`Err | `Ignore]
      }
  | ActFilterMap of ('a -> 'a option)

let use = ActOnExistence {if_existing = `Keep; if_absent = `Err}
let pass = ActOnExistence {if_existing = `Keep; if_absent = `Ignore}
let hide = ActOnExistence {if_existing = `Hide; if_absent = `Err}
let ignore = ActOnExistence {if_existing = `Hide; if_absent = `Ignore}

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

let none = PatAct hide
let any = PatAct use
let wildcard = PatRootSplit {on_root = PatAct ignore; on_children = any}
let root = PatRootSplit {on_root = any; on_children = PatAct ignore}
let scope prefix on_subtree =
  PatScopeSplit {prefix; prefix_replacement = None; on_subtree; on_others = PatAct pass}
let renaming_scope prefix prefix_replacement on_subtree =
  PatScopeSplit {prefix; prefix_replacement = Some prefix_replacement; on_subtree; on_others = PatAct pass}
let seq pats = PatSeq pats
let only x = scope x root
let renaming x x' = renaming_scope x x' root
let prefix x = scope x any
let renaming_prefix x x' = renaming_scope x x' any
let attr a p = seq [PatAct (ActFilterMap (fun _ -> Some a)); p]
let union l = PatUnion l
let except_root = PatRootSplit {on_root = PatAct hide; on_children = PatAct pass}
let except x = scope x except_root
let except_prefix x = scope x none
