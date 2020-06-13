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
let rec scope =
  function
  | [] -> fun p -> p
  | s ->
    function
    | PatScope (s2, None, p) -> scope (s @ s2) p
    | p -> PatScope (s, None, p)
let rec renaming_scope s s' =
  if s = [] && s' = [] then
    fun p -> p
  else
    function
    | PatScope (s2, Some s'2, p) -> renaming_scope (s @ s2) (s' @ s'2) p
    | p -> PatScope (s, Some s', p)
let seq acts = PatSeq acts
let seq_filter acts = inv @@ seq @@ List.map inv acts
let none = seq []
let any = inv none
let id x = scope x root
let renaming x x' = renaming_scope x x' root
let prefix x = scope x any
let renaming_prefix x x' = renaming_scope x x' any
let rec attr a =
  function
  | PatAttr (_, p) -> attr a p
  | p -> PatAttr (a, p)
let join l = PatJoin l
let unsafe_meet l = inv @@ join @@ List.map inv l
let meet =
  function
  | [] -> invalid_arg "Pattern.meet: empty list"
  | l -> unsafe_meet l
let hide x = inv @@ id x
let hide_prefix x = inv @@ prefix x
let unsafe_inv = inv
