type path = string list

type 'a act =
  | ActCheckExistence of
      { if_existing : [`Error | `Keep | `Hide]
      ; if_absent : [`Error | `Ignored]
      }
  | ActFilterMap of ('a -> 'a option)

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
