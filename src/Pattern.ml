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
  | PatMeet of pattern list
[@@deriving show]

let wildcard = PatWildcard
let root = PatInv PatWildcard
let id p = PatScope (p, None, PatAny)
let renaming p r = PatScope (p, Some r, PatAny)
let any = PatAny
let none = PatInv PatAny
let scope p a = PatScope (p, None, a)
let renaming_scope p r a = PatScope (p, Some r, a)
let seq acts = PatSeq acts
let inv a = PatInv a
let public a = PatExport (`Public, a)
let private_ a = PatExport (`Private, a)
let join l = PatJoin l
let meet l = PatMeet l
