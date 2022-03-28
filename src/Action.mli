type _ Effect.t += BindingNotFound : Pattern.path -> unit Effect.t

val run :
  ?rev_prefix:Pattern.path ->
  merger:(rev_path:Pattern.path -> 'a -> 'a -> 'a) ->
  unit Pattern.t -> 'a Trie.t -> 'a Trie.t

val run_with_hooks :
  ?rev_prefix:Pattern.path ->
  merger:(rev_path:Pattern.path -> 'a -> 'a -> 'a) ->
  hooks:('hook -> rev_prefix:Pattern.path -> 'a Trie.t -> 'a Trie.t) ->
  'hook Pattern.t -> 'a Trie.t -> 'a Trie.t
