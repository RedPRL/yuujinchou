type 'hook t =
  | P_only of Trie.path
  | P_none
  | P_in of Trie.path * 'hook t
  | P_renaming of Trie.path * Trie.path
  | P_seq of 'hook t list
  | P_union of 'hook t list
  | P_hook of 'hook

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
