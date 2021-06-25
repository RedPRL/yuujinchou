open Yuujinchou

module DataSet : Set.S with type elt = int

type env = int Trie.t

val import : env -> unit Pattern.t -> env -> env
val select : env -> unit Pattern.t -> DataSet.t
