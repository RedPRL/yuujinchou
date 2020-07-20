type path = string list

type ('a, 'b) act =
  | ActSimple of
      { if_existing : [`Error of 'b | `Keep | `Hide]
      ; if_absent : [`Error of 'b | `Ignored]
      }
  | ActMap of ('a -> 'a)
  | ActFilterMap of ('a -> [`Error of 'b | `Keep of 'a | `Hide])

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
  | PatTry of ('a, 'b) pattern * ('b -> ('a, 'b) pattern)
