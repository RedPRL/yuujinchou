open Yuujinchou

module DataSet : Set.S with type elt = int

type env = int Trie.t

type empty = |
val import : env -> empty Pattern.t -> env -> env
val select : env -> empty Pattern.t -> DataSet.t
