type path = string list
[@@deriving show]

type 'a pattern =
  | PatWildcard
  | PatScope of path * path option * 'a pattern
  | PatSeq of 'a pattern list
  | PatInv of 'a pattern
  | PatJoin of 'a pattern list
  | PatAttr of 'a * 'a pattern
[@@deriving show]

let inv =
  function
  | PatInv p -> p
  | p -> PatInv p
let wildcard = PatWildcard
let root = inv wildcard
let scope s p = PatScope (s, None, p)
let renaming_scope s s' p = PatScope (s, Some s', p)
let seq acts = PatSeq acts
let seq_filter acts = inv @@ seq @@ List.map inv acts
let none = seq []
let any = inv none
let id x = scope x root
let renaming x x' = renaming_scope x x' root
let prefix x = scope x any
let renaming_prefix x x' = renaming_scope x x' any
let attr a p = PatAttr (a, p)
let join l = PatJoin l
let unsafe_meet l = inv @@ join @@ List.map inv l
let meet =
  function
  | [] -> invalid_arg "Pattern.meet: empty list"
  | l -> unsafe_meet l
let skip x = inv @@ id x
let skip_prefix x = inv @@ prefix x
let unsafe_inv = inv
