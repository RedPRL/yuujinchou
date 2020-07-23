type path = Pattern.path

type error = BindingNotFound of path
val pp_error : Format.formatter -> error -> unit

val run : ('a -> 'a -> 'a) -> 'a Pattern.t -> 'a Trie.t -> ('a Trie.t, error) result
