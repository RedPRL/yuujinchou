type path = string list

type 'a pattern =
  | PatRootSplit of
      { root : [`Keep | `Hide | `Unchanged]
      ; children : [`Keep | `Hide | `Unchanged]
      }
  | PatScopeSplit of
      { prefix : path
      ; replacement : path option
      ; subtree : 'a pattern
      ; others : [`Keep | `Hide | `Unchanged]
      }
  | PatSeq of 'a pattern list
  | PatUnion of 'a pattern list
  | PatAttrMod of ('a -> [`Keep of 'a | `Hide]) * 'a pattern
