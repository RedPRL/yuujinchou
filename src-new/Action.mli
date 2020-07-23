type path = Pattern.path

type error = BindingNotFound of path

val run : ('a -> 'a -> 'a) -> 'a Pattern.pattern -> 'a Trie.t -> ('a Trie.t, error) result
