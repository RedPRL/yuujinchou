type path = string list

type act =
  { if_existing : [`Error | `Keep | `Hide]
  ; if_absent : [`Error | `Ignored]
  }

type 'a pattern =
  | PatRootSplit of
      { on_root : act
      ; on_children : act
      }
  | PatScopeSplit of
      { prefix : path
      ; prefix_replacement : path option
      ; on_subtree : 'a pattern
      ; on_others : 'a pattern
      }
  | PatSeq of 'a pattern list
  | PatUnion of 'a pattern list
  | PatModify of ('a -> [`Error | `Keep of 'a | `Hide])
  | PatTry of 'a pattern * 'a pattern
