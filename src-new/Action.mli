type error = BindingNotFound

val run : ('a -> 'a -> 'a) -> 'a Pattern.pattern -> 'a Trie.t -> ('a Trie.t, string list * error) result
