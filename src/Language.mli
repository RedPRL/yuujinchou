type ('hook, 'kind) t_ =
  | M_only : Trie.path -> ('hook, [< `Modifier | `Selector]) t_
  | M_none : ('hook, [< `Modifier | `Selector]) t_
  | M_in : Trie.path * ('hook, [< `Modifier] as 'kind) t_ -> ('hook, 'kind) t_
  | M_renaming : Trie.path * Trie.path -> ('hook, [< `Modifier]) t_
  | M_seq : ('hook, [< `Modifier] as 'kind) t_ list -> ('hook, 'kind) t_
  | M_union : ('hook, [< `Modifier | `Selector] as 'kind) t_ list -> ('hook, 'kind) t_
  | M_hook : 'hook -> ('hook, [< `Modifier | `Selector]) t_

type ('hook, 'kind) t = ('hook, [< `Modifier | `Selector] as 'kind) t_
type 'hook modifier = ('hook, [`Modifier]) t
type 'hook selector = ('hook, [`Selector]) t

val equal : ('hook -> 'hook -> bool) -> ('hook, 'kind) t -> ('hook, 'kind) t -> bool

val any : ('hook, 'kind) t

val only : Trie.path -> ('hook, 'kind) t

val none : ('hook, 'kind) t
val except : Trie.path -> 'hook modifier
val in_ : Trie.path -> 'hook modifier -> 'hook modifier

val renaming : Trie.path -> Trie.path -> 'hook modifier

val seq : 'hook modifier list -> 'hook modifier

val union : ('hook, 'kind) t list -> ('hook, 'kind) t

val hook : 'hook -> ('hook, 'kind) t

val dump : (Format.formatter -> 'hook -> unit) -> Format.formatter -> ('hook, 'kind) t -> unit
