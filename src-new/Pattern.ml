type path = string list

type ('a, 'b) act =
  | ActCheckExistence of
      { if_existing : [`Error of 'b | `Keep | `Hide]
      ; if_absent : [`Error of 'b | `Ignored]
      }
  | ActFilterMap of ('a -> 'a option)

type ('a, 'b) pattern =
  | PatAct of ('a, 'b) act
  | PatRootSplit of
      { on_root : ('a, 'b) pattern
      ; on_children : ('a, 'b) pattern
      }
  | PatScopeSplit of
      { prefix : path
      ; prefix_replacement : path option
      ; on_subtree : ('a, 'b) pattern
      ; on_others : ('a, 'b) pattern
      }
  | PatSeq of ('a, 'b) pattern list
  | PatUnion of ('a, 'b) pattern list
