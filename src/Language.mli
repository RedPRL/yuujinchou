(* See Yuujinchou.mli for documentation. *)

type 'hook t =
  | M_assert_nonempty
  | M_in of Trie.path * 'hook t
  | M_renaming of Trie.path * Trie.path
  | M_seq of 'hook t list
  | M_union of 'hook t list
  | M_hook of 'hook

include LanguageSigs.S with type 'hook t := 'hook t
