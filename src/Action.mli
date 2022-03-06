type nonrec ('a, 'error) result = ('a, [> `BindingNotFound of Pattern.path] as 'error) result

val run :
  ?rev_prefix:Pattern.path ->
  union:(rev_path:Pattern.path -> 'a -> 'a -> ('a, 'error) result) ->
  unit Pattern.t -> 'a Trie.t -> ('a Trie.t, 'error) result

val run_with_hooks :
  ?rev_prefix:Pattern.path ->
  union:(rev_path:Pattern.path -> 'a -> 'a -> ('a, 'error) result) ->
  hooks:('hook -> rev_prefix:Pattern.path -> 'a Trie.t -> ('a Trie.t, 'error) result) ->
  'hook Pattern.t -> 'a Trie.t -> ('a Trie.t, 'error) result

val pp_path : Format.formatter -> Pattern.path -> unit
