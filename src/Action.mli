type path = Pattern.path
val pp_path : Format.formatter -> path -> unit

val run_with_custom :
  ?rev_prefix:path ->
  union:(rev_path:path -> 'a -> 'a -> 'a) ->
  custom:(rev_path:path -> 'f -> 'a -> 'a option) ->
  ('a, 'f) Pattern.t -> 'a Trie.t -> ('a Trie.t, [> `BindingNotFound of path]) result

val run :
  ?rev_prefix:path ->
  union:(rev_path:path -> 'a -> 'a -> 'a) ->
  ('a, unit) Pattern.t -> 'a Trie.t -> ('a Trie.t, [> `BindingNotFound of path]) result
