open StdLabels
open Yuujinchou
module LT = ListAsTrie
module Q = QCheck2

let cmp_path = List.compare ~cmp:String.compare
let cmp (p1, _) (p2, _) = cmp_path p1 p2

let rawlist_to_list l = ListAsTrie.of_seq @@ List.to_seq l
let list_to_rawlist l = List.of_seq @@ ListAsTrie.to_seq l
let rawlist_to_untagged_list l = ListAsTrie.Untagged.of_seq @@ List.to_seq l
let untagged_list_to_rawlist l = List.of_seq @@ ListAsTrie.Untagged.to_seq l

let print_path = Q.Print.(list string)
let print_bwd_path = Q.Print.contramap Bwd.BwdLabels.to_list print_path
let print_tagged = Q.Print.(pair int int)
let print_list = Q.Print.(contramap list_to_rawlist (list @@ pair print_path print_tagged))
let print_untagged_list = Q.Print.(contramap untagged_list_to_rawlist (list @@ pair print_path int))

let gen_path = Q.Gen.(small_list @@ small_string ~gen:printable)
let gen_bwd_path = Q.Gen.map Bwd.BwdLabels.of_list gen_path
let gen_tagged = Q.Gen.(pair int small_int)
let gen_list = Q.Gen.map (fun l -> rawlist_to_list @@ List.sort_uniq ~cmp l)
    Q.Gen.(small_list @@ pair gen_path gen_tagged)
let gen_untagged_list = Q.Gen.map (fun l -> rawlist_to_untagged_list @@ List.sort_uniq ~cmp l)
    Q.Gen.(small_list @@ pair gen_path int)

let obs_path = Q.Observable.(list string)
let obs_bwd_path = Q.Observable.contramap Bwd.BwdLabels.to_list obs_path
let obs_tagged = Q.Observable.(pair int int)
let obs_list = Q.Observable.(contramap list_to_rawlist @@ list @@ pair obs_path (pair int int))

let of_list l = Trie.of_seq @@ ListAsTrie.to_seq l
let to_list t = ListAsTrie.of_seq @@ Trie.to_seq t
let of_untagged_list l = Trie.Untagged.of_seq @@ ListAsTrie.Untagged.to_seq l
let to_untagged_list t = ListAsTrie.Untagged.of_seq @@ Trie.Untagged.to_seq t

(* for iter *)
let bag_create () = ref []
let bag_push x l = l := x :: !l
let bag_eq l1 l2 = List.stable_sort ~cmp:compare l1 = List.stable_sort ~cmp:compare l2

let count = 100

let test_empty =
  Q.Test.make ~count ~name:"empty" Q.Gen.unit ~print:Q.Print.unit
    (fun () -> to_list Trie.empty = ListAsTrie.empty)
let test_is_empty =
  Q.Test.make ~count ~name:"is_empty" gen_list ~print:print_list
    (fun l -> Trie.is_empty (of_list l) = ListAsTrie.is_empty l)
let test_root =
  Q.Test.make ~count ~name:"root" gen_tagged ~print:print_tagged
    (fun x -> to_list (Trie.root x) = ListAsTrie.root x)
let test_root_opt =
  Q.Test.make ~count ~name:"root_opt" Q.Gen.(opt gen_tagged) ~print:Q.Print.(option print_tagged)
    (fun x -> to_list (Trie.root_opt x) = ListAsTrie.root_opt x)
let test_prefix =
  Q.Test.make ~count ~name:"prefix" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> to_list (Trie.prefix p (of_list l)) = ListAsTrie.prefix p l)
let test_singleton =
  Q.Test.make ~count ~name:"singleton" Q.Gen.(pair gen_path gen_tagged)
    ~print:Q.Print.(pair print_path print_tagged)
    (fun (p, x) -> to_list (Trie.singleton (p, x)) = ListAsTrie.singleton (p, x))

(* Trie.equal is not tested *)

let test_find_subtree =
  Q.Test.make ~count ~name:"find_subtree" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> to_list (Trie.find_subtree p (of_list l)) = ListAsTrie.find_subtree p l)
let test_find_singleton =
  Q.Test.make ~count ~name:"find_singleton" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> Trie.find_singleton p (of_list l) = ListAsTrie.find_singleton p l)
let test_find_root =
  Q.Test.make ~count ~name:"find_root" gen_list ~print:print_list
    (fun l -> Trie.find_root (of_list l) = ListAsTrie.find_root l)

let test_iter =
  Q.Test.make ~count ~name:"iter" Q.Gen.(pair (opt gen_bwd_path) gen_list)
    ~print:Q.Print.(pair (option print_bwd_path) print_list)
    (fun (prefix, l) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       Trie.iter ?prefix (fun path x -> bag_push (path, x) calls1) (of_list l);
       ListAsTrie.iter ?prefix (fun path x -> bag_push (path, x) calls2) l;
       bag_eq !calls1 !calls2)
let test_map =
  Q.Test.make ~count ~name:"map"
    Q.Gen.(triple (opt gen_bwd_path) (Q.fun2 obs_bwd_path obs_tagged gen_tagged) gen_list)
    ~print:Q.Print.(triple (option print_bwd_path) Q.Fn.print print_list)
    (fun (prefix, Fun (_, f), l) ->
       to_list (Trie.map ?prefix f (of_list l))
       =
       ListAsTrie.map ?prefix f l)
let test_filter =
  Q.Test.make ~count ~name:"filter"
    Q.Gen.(triple (opt gen_bwd_path) (Q.fun2 obs_bwd_path obs_tagged bool) gen_list)
    ~print:Q.Print.(triple (option print_bwd_path) Q.Fn.print print_list)
    (fun (prefix, Fun (_, f), l) ->
       to_list (Trie.filter ?prefix f (of_list l))
       =
       ListAsTrie.filter ?prefix f l)
let test_filter_map =
  Q.Test.make ~count ~name:"filter_map"
    Q.Gen.(triple (opt gen_bwd_path) (Q.fun2 obs_bwd_path Q.Observable.(pair int int) (opt gen_tagged)) gen_list)
    ~print:Q.Print.(triple (option print_bwd_path) Q.Fn.print print_list)
    (fun (prefix, Fun (_, f), l) ->
       to_list
         (Trie.filter_map ?prefix f (of_list l))
       =
       ListAsTrie.filter_map ?prefix f l)

let test_update_subtree =
  Q.Test.make ~count ~name:"update_subtree"
    Q.Gen.(triple gen_path (Q.fun1 obs_list gen_list) gen_list)
    ~print:Q.Print.(triple print_path Q.Fn.print print_list)
    (fun (p, Fun (_, f), l) ->
       to_list
         (Trie.update_subtree p
            (fun t -> of_list (f (to_list t)))
            (of_list l))
       =
       ListAsTrie.update_subtree p f l)
let test_update_singleton =
  Q.Test.make ~count ~name:"update_singleton"
    Q.Gen.(triple gen_path (Q.fun1 Q.Observable.(option (pair int int)) (opt gen_tagged)) gen_list)
    ~print:Q.Print.(triple print_path Q.Fn.print print_list)
    (fun (p, Fun (_, f), l) ->
       to_list
         (Trie.update_singleton p f (of_list l))
       =
       ListAsTrie.update_singleton p f l)
let test_update_root =
  Q.Test.make ~count ~name:"update_root"
    Q.Gen.(pair (Q.fun1 Q.Observable.(option (pair int int)) (opt gen_tagged)) gen_list)
    ~print:Q.Print.(pair Q.Fn.print print_list)
    (fun (Fun (_, f), l) ->
       to_list
         (Trie.update_root f (of_list l))
       =
       ListAsTrie.update_root f l)

let gen_merge = Q.fun3 obs_bwd_path obs_tagged obs_tagged gen_tagged

let test_union =
  Q.Test.make ~count ~name:"union"
    Q.Gen.(quad (opt gen_bwd_path) gen_merge gen_list gen_list)
    ~print:Q.Print.(quad (option print_bwd_path) Q.Fn.print print_list print_list)
    (fun (prefix, Fun (_, f), l1, l2) ->
       to_list
         (Trie.union ?prefix f (of_list l1) (of_list l2))
       =
       ListAsTrie.union ?prefix f l1 l2)
let test_union_subtree =
  Q.Test.make ~count ~name:"union_subtree"
    Q.Gen.(quad (opt gen_bwd_path) gen_merge gen_list (pair gen_path gen_list))
    ~print:Q.Print.(quad (option print_bwd_path) Q.Fn.print print_list (pair print_path print_list))
    (fun (prefix, Fun (_, f), l1, (pre, l2)) ->
       to_list
         (Trie.union_subtree ?prefix f (of_list l1) (pre, of_list l2))
       =
       ListAsTrie.union_subtree ?prefix f l1 (pre, l2))
let test_union_singleton =
  Q.Test.make ~count ~name:"union_singleton"
    Q.Gen.(quad (opt gen_bwd_path) gen_merge gen_list (pair gen_path gen_tagged))
    ~print:Q.Print.(quad (option print_bwd_path) Q.Fn.print print_list (pair print_path print_tagged))
    (fun (prefix, Fun (_, f), l1, b) ->
       to_list
         (Trie.union_singleton ?prefix f (of_list l1) b)
       =
       ListAsTrie.union_singleton ?prefix f l1 b)
let test_union_root =
  Q.Test.make ~count ~name:"union_root"
    Q.Gen.(quad (opt gen_bwd_path) gen_merge gen_list gen_tagged)
    ~print:Q.Print.(quad (option print_bwd_path) Q.Fn.print print_list print_tagged)
    (fun (prefix, Fun (_, f), l1, x) ->
       to_list
         (Trie.union_root ?prefix f (of_list l1) x)
       =
       ListAsTrie.union_root ?prefix f l1 x)

let test_detach_subtree =
  Q.Test.make ~count ~name:"detach_subtree" Q.Gen.(pair gen_path gen_list)
    ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) ->
       (fun (t1, t2) -> to_list t1, to_list t2)
         (Trie.detach_subtree p (of_list l))
       =
       ListAsTrie.detach_subtree p l)
let test_detach_singleton =
  Q.Test.make ~count ~name:"detach_singleton" Q.Gen.(pair gen_path gen_list)
    ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) ->
       (fun (x, t) -> x, to_list t)
         (Trie.detach_singleton p (of_list l))
       =
       ListAsTrie.detach_singleton p l)
let test_detach_root =
  Q.Test.make ~count ~name:"detach_root" gen_list ~print:print_list
    (fun l ->
       (fun (x, t) -> x, to_list t)
         (Trie.detach_root (of_list l))
       =
       ListAsTrie.detach_root l)

let test_to_seq =
  Q.Test.make ~count ~name:"to_seq" Q.Gen.(pair (opt gen_bwd_path) gen_list)
    ~print:Q.Print.(pair (option print_bwd_path) print_list)
    (fun (prefix, l) ->
       List.of_seq (Trie.to_seq ?prefix (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq ?prefix l))
let test_to_seq_with_bwd_paths =
  Q.Test.make ~count ~name:"to_seq_with_bwd_paths" Q.Gen.(pair (opt gen_bwd_path) gen_list)
    ~print:Q.Print.(pair (option print_bwd_path) print_list)
    (fun (prefix, l) ->
       List.of_seq (Trie.to_seq_with_bwd_paths ?prefix (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq_with_bwd_paths ?prefix l))
let test_to_seq_values =
  Q.Test.make ~count ~name:"to_seq_values" gen_list ~print:print_list
    (fun l ->
       List.of_seq (Trie.to_seq_values (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq_values l))
let test_to_seq_values_with_tags =
  Q.Test.make ~count ~name:"to_seq_values_with_tags" gen_list ~print:print_list
    (fun l ->
       List.of_seq (Trie.to_seq_values_with_tags (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq_values_with_tags l))
let test_of_seq =
  Q.Test.make ~count ~name:"of_seq"
    Q.Gen.(small_list @@ pair gen_path gen_tagged)
    ~print:Q.Print.(list @@ pair print_path print_tagged)
    (fun l ->
       to_list (Trie.of_seq (List.to_seq l))
       =
       ListAsTrie.of_seq (List.to_seq l))
let test_of_seq_with_merger =
  Q.Test.make ~count ~name:"of_seq_with_merger"
    Q.Gen.(triple (opt gen_bwd_path) gen_merge (small_list @@ pair gen_path gen_tagged))
    ~print:Q.Print.(triple (option print_bwd_path) Q.Fn.print (list @@ pair print_path print_tagged))
    (fun (prefix, Fun (_, f), l) ->
       to_list (Trie.of_seq_with_merger ?prefix f (List.to_seq l))
       =
       ListAsTrie.of_seq_with_merger ?prefix f (List.to_seq l))
let test_tag =
  Q.Test.make ~count ~name:"tag" Q.Gen.(pair int gen_untagged_list) ~print:Q.Print.(pair int print_untagged_list)
    (fun (t, l) -> to_list (Trie.tag t (of_untagged_list l)) = ListAsTrie.tag t l)
let test_untag =
  Q.Test.make ~count ~name:"untag" gen_list ~print:print_list
    (fun l -> to_untagged_list (Trie.untag (of_list l)) = ListAsTrie.untag l)
let test_retag =
  Q.Test.make ~count ~name:"retag" Q.Gen.(pair int gen_list) ~print:Q.Print.(pair int print_list)
    (fun (t, l) -> to_list (Trie.retag t (of_list l)) = ListAsTrie.retag t l)
let test_retag_subtree =
  Q.Test.make ~count ~name:"retag_subtree" Q.Gen.(triple gen_path int gen_list) ~print:Q.Print.(triple print_path int print_list)
    (fun (p, t, l) -> to_list (Trie.retag_subtree p t (of_list l)) = ListAsTrie.retag_subtree p t l)

module Untagged =
struct
  let test_to_seq =
    Q.Test.make ~count ~name:"Untagged.to_seq" Q.Gen.(pair (opt gen_bwd_path) gen_untagged_list)
      ~print:Q.Print.(pair (option print_bwd_path) print_untagged_list)
      (fun (prefix, l) ->
         List.of_seq (Trie.Untagged.to_seq ?prefix (of_untagged_list l))
         =
         List.of_seq (ListAsTrie.Untagged.to_seq ?prefix l))
  let test_to_seq_with_bwd_paths =
    Q.Test.make ~count ~name:"Untagged.to_seq_with_bwd_paths" Q.Gen.(pair (opt gen_bwd_path) gen_untagged_list)
      ~print:Q.Print.(pair (option print_bwd_path) print_untagged_list)
      (fun (prefix, l) ->
         List.of_seq (Trie.Untagged.to_seq_with_bwd_paths ?prefix (of_untagged_list l))
         =
         List.of_seq (ListAsTrie.Untagged.to_seq_with_bwd_paths ?prefix l))
  let test_to_seq_values =
    Q.Test.make ~count ~name:"Untagged.to_seq_values" gen_untagged_list ~print:print_untagged_list
      (fun l ->
         List.of_seq (Trie.Untagged.to_seq_values (of_untagged_list l))
         =
         List.of_seq (ListAsTrie.Untagged.to_seq_values l))
  let test_of_seq =
    Q.Test.make ~count ~name:"Untagged.of_seq"
      Q.Gen.(small_list @@ pair gen_path int)
      ~print:Q.Print.(list @@ pair print_path int)
      (fun l ->
         to_untagged_list (Trie.Untagged.of_seq (List.to_seq l))
         =
         ListAsTrie.Untagged.of_seq (List.to_seq l))
end

let () =
  exit @@
  QCheck_base_runner.run_tests ~colors:true ~verbose:true ~long:true
    [ test_empty
    ; test_is_empty
    ; test_root
    ; test_root_opt
    ; test_prefix
    ; test_singleton
    ; test_find_subtree
    ; test_find_singleton
    ; test_find_root
    ; test_iter
    ; test_map
    ; test_filter
    ; test_filter_map
    ; test_update_subtree
    ; test_update_singleton
    ; test_update_root
    ; test_union
    ; test_union_subtree
    ; test_union_singleton
    ; test_union_root
    ; test_detach_subtree
    ; test_detach_singleton
    ; test_detach_root
    ; test_to_seq
    ; test_to_seq_with_bwd_paths
    ; test_to_seq_values
    ; test_to_seq_values_with_tags
    ; test_of_seq
    ; test_of_seq_with_merger
    ; test_tag
    ; test_untag
    ; test_retag
    ; test_retag_subtree
    ; Untagged.test_to_seq
    ; Untagged.test_to_seq_with_bwd_paths
    ; Untagged.test_to_seq_values
    ; Untagged.test_of_seq
    ]
