open Pattern

type error = BindingNotFound

val run : ('a -> 'a -> 'a) -> 'a pattern -> 'a Trie.t -> ('a Trie.t, (string list * error)) result
