type path = Pattern.path
val pp_path : Format.formatter -> path -> unit

val run_with_hooks :
  ?rev_prefix:path ->
  union:(rev_path:path -> 'a -> 'a -> 'a) ->
  hooks:('hook -> rev_prefix:path -> 'a Trie.t -> ('a Trie.t, [> `BindingNotFound of path] as 'error) result) ->
  'hook Pattern.t -> 'a Trie.t -> ('a Trie.t, 'error) result

val run :
  ?rev_prefix:path ->
  union:(rev_path:path -> 'a -> 'a -> 'a) ->
  unit Pattern.t -> 'a Trie.t -> ('a Trie.t, [> `BindingNotFound of path]) result
