type 'hook t =
  | M_none
  | M_in of Trie.path * 'hook t
  | M_renaming of Trie.path * Trie.path
  | M_seq of 'hook t list
  | M_union of 'hook t list
  | M_hook of 'hook

val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

val any : 'hook t

val only : Trie.path -> 'hook t

val none : 'hook t
val except : Trie.path -> 'hook t
val in_ : Trie.path -> 'hook t -> 'hook t

val renaming : Trie.path -> Trie.path -> 'hook t

val seq : 'hook t list -> 'hook t

val union : 'hook t list -> 'hook t

val hook : 'hook -> 'hook t

val dump : (Format.formatter -> 'hook -> unit) -> Format.formatter -> 'hook t -> unit
