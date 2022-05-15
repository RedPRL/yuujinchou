open StdLabels

open Bwd
open BwdNotation

open Language

module type Param =
sig
  type data
  type hook
  type caller
  val compare_data : data -> data -> int
end

module type S =
sig
  include Param

  module DataSet : Set.S with type elt = data

  type _ Effect.t +=
    | BindingNotFound : {caller : caller option; prefix : Trie.bwd_path} -> unit Effect.t
    | Hook : {caller : caller option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> DataSet.t Effect.t

  val exec : ?caller:caller -> ?prefix:Trie.bwd_path -> hook selector -> data Trie.t -> DataSet.t
end

module Make (P : Param) : S with type data = P.data and type hook = P.hook and type caller = P.caller =
struct
  include P

  module DataSet = Set.Make(struct type t = data let compare = compare_data end)

  type _ Effect.t +=
    | BindingNotFound : {caller : caller option; prefix : Trie.bwd_path} -> unit Effect.t
    | Hook : {caller : caller option; prefix : Trie.bwd_path; hook : hook; input : data Trie.t} -> DataSet.t Effect.t

  let to_set t = DataSet.of_seq (Trie.to_seq_values t)

  let check_nonempty ~caller ~prefix t =
    if Trie.is_empty t then
      Effect.perform @@ BindingNotFound {caller; prefix}
  let do_hook ~caller ~hook ~prefix t =
    Effect.perform @@ Hook {caller; prefix; hook; input=t}

  let exec ?caller ?(prefix=Emp) =
    let rec go ~prefix (s : 'kind Language.selector) t =
      match s with
      | M_only p ->
        let t = Trie.find_subtree p t in
        check_nonempty ~caller ~prefix:(prefix <>< p) t;
        to_set t
      | M_union fs ->
        let f ss f = DataSet.union ss (go ~prefix f t) in
        List.fold_left ~f ~init:DataSet.empty fs
      | M_hook hook -> do_hook ~caller ~hook ~prefix t
    in go ~prefix
end
