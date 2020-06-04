type exportability = [`Public | `Private]
[@@deriving show]

type path = string list
[@@deriving show]

type pattern =
  | PatAny
  | PatWildcard
  | PatScope of path * path option * pattern
  | PatSeq of pattern list
  | PatInv of pattern
  | PatExport of exportability * pattern
  | PatJoin of pattern list
[@@deriving show]

let inv p = PatInv p
let any = PatAny
let wildcard = PatWildcard
let root = inv wildcard
let scope s p = PatScope (s, None, p)
let renaming_scope s s' p = PatScope (s, Some s', p)
let id x = scope x any
let renaming x x' = renaming_scope x x' any
let none = inv any
let seq acts = PatSeq acts
let public a = PatExport (`Public, a)
let private_ a = PatExport (`Private, a)
let join l = PatJoin l
let meet l = inv @@ join @@ List.map join l
