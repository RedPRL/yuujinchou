open StdLabels
open Algaeff.StdlibShim

open Bwd
open BwdNotation

open Language

type source = Modifier.source

module type Param =
sig
  type data
  type hook
  val compare_data : data -> data -> int
end

module type S =
sig
  include Param


  module DataSet : Set.S with type elt = data

  type _ Effect.t +=
    | BindingNotFound : {source : source option; prefix : Trie.bwd_path} -> unit Effect.t
    | Hook : {source : source option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> DataSet.t Effect.t

  val exec : ?source:source -> ?prefix:Trie.bwd_path -> hook selector -> data Trie.t -> DataSet.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook =
struct
  include P

  module DataSet = Set.Make(struct type t = data let compare = compare_data end)

  type _ Effect.t +=
    | BindingNotFound : {source : source option; prefix : Trie.bwd_path} -> unit Effect.t
    | Hook : {source : source option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> DataSet.t Effect.t

  let to_set t = DataSet.of_seq (Trie.to_seq_values t)

  let check_nonempty ~source ~prefix t =
    if Trie.is_empty t then
      Effect.perform @@ BindingNotFound {source; prefix}
  let do_hook ~source ~hook ~prefix t =
    Effect.perform @@ Hook {source; prefix; hook; input=t}

  let exec ?source ?(prefix=Emp) =
    let rec go ~prefix (s : 'kind Language.selector) t =
      match s with
      | M_only p ->
        let t = Trie.find_subtree p t in
        check_nonempty ~source ~prefix:(prefix <>< p) t;
        to_set t
      | M_union fs ->
        let f ss f = DataSet.union ss (go ~prefix f t) in
        List.fold_left ~f ~init:DataSet.empty fs
      | M_hook hook -> do_hook ~source ~hook ~prefix t
    in go ~prefix
end
