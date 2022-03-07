open StdLabels
open Yuujinchou
module LT = ListAsTrie
module Q = QCheck2

let cmp_path = List.compare ~cmp:String.compare
let cmp (p1, _) (p2, _) = cmp_path p1 p2

let seq_to_list s = ListAsTrie.of_seq (fun ~rev_path:_ _ _ -> failwith "Conflicting bindings") s
let rawlist_to_list l = seq_to_list @@ List.to_seq l
let list_to_rawlist l = List.of_seq @@ ListAsTrie.to_seq l

let print_path = Q.Print.(list string)
let print_list = Q.Print.(contramap list_to_rawlist (list @@ pair print_path int))

let gen_path = Q.Gen.(small_list @@ small_string ~gen:printable)
let gen_list = Q.Gen.map (fun l -> rawlist_to_list @@ List.sort_uniq ~cmp l)
    Q.Gen.(small_list @@ pair gen_path int)

let obs_path = Q.Observable.(list string)
let obs_list = Q.Observable.(contramap list_to_rawlist @@ list @@ pair obs_path int)

let of_list l = Trie.of_seq (fun ~rev_path:_ _ _ -> failwith "Conflicting bindings") @@ ListAsTrie.to_seq l
let to_list t = seq_to_list @@ Trie.to_seq t

(* for iteri *)
let bag_create () = ref []
let bag_push x l = l := x :: !l
let bag_eq l1 l2 = List.stable_sort ~cmp:compare l1 = List.stable_sort ~cmp:compare l2

let count = 100

let test_empty =
  Q.Test.make ~count ~name:"empty" Q.Gen.unit
    (fun () -> to_list Trie.empty = ListAsTrie.empty)
let test_is_empty =
  Q.Test.make ~count ~name:"is_empty" gen_list ~print:print_list
    (fun l -> Trie.is_empty (of_list l) = ListAsTrie.is_empty l)
let test_root =
  Q.Test.make ~count ~name:"root" Q.Gen.int ~print:Q.Print.int
    (fun x -> to_list (Trie.root x) = ListAsTrie.root x)
let test_root_opt =
  Q.Test.make ~count ~name:"root_opt" Q.Gen.(opt int) ~print:Q.Print.(option int)
    (fun x -> to_list (Trie.root_opt x) = ListAsTrie.root_opt x)
let test_prefix =
  Q.Test.make ~count ~name:"prefix" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> to_list (Trie.prefix p (of_list l)) = ListAsTrie.prefix p l)
let test_singleton =
  Q.Test.make ~count ~name:"singleton" Q.Gen.(pair gen_path int) ~print:Q.Print.(pair print_path int)
    (fun (p, x) -> to_list (Trie.singleton (p, x)) = ListAsTrie.singleton (p, x))
let test_find_subtree =
  Q.Test.make ~count ~name:"find_subtree" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> to_list (Trie.find_subtree p (of_list l)) = ListAsTrie.find_subtree p l)
let test_find_singleton =
  Q.Test.make ~count ~name:"find_singleton" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> Trie.find_singleton p (of_list l) = ListAsTrie.find_singleton p l)
let test_find_root =
  Q.Test.make ~count ~name:"find_root" gen_list ~print:print_list
    (fun l -> Trie.find_root (of_list l) = ListAsTrie.find_root l)
let test_iteri =
  Q.Test.make ~count ~name:"iteri" Q.Gen.(pair (opt gen_path) gen_list)
    ~print:Q.Print.(pair (option print_path) print_list)
    (fun (rev_prefix, l) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       Trie.iteri ?rev_prefix (fun ~rev_path x -> bag_push (rev_path, x) calls1) (of_list l);
       ListAsTrie.iteri ?rev_prefix (fun ~rev_path x -> bag_push (rev_path, x) calls2) l;
       bag_eq !calls1 !calls2)
let test_mapi =
  Q.Test.make ~count ~name:"mapi"
    Q.Gen.(triple (opt gen_path) (Q.fun2 obs_path Q.Observable.int int) gen_list)
    ~print:Q.Print.(triple (option print_path) Q.Fn.print print_list)
    (fun (rev_prefix, Fun (_, f), l) ->
       to_list (Trie.mapi ?rev_prefix (fun ~rev_path -> f rev_path) (of_list l))
       =
       ListAsTrie.mapi ?rev_prefix (fun ~rev_path -> f rev_path) l;)
let test_filteri =
  Q.Test.make ~count ~name:"filteri"
    Q.Gen.(triple (opt gen_path) (Q.fun2 obs_path Q.Observable.int bool) gen_list)
    ~print:Q.Print.(triple (option print_path) Q.Fn.print print_list)
    (fun (rev_prefix, Fun (_, f), l) ->
       to_list (Trie.filteri ?rev_prefix (fun ~rev_path -> f rev_path) (of_list l))
       =
       ListAsTrie.filteri ?rev_prefix (fun ~rev_path -> f rev_path) l)
let test_filter_mapi =
  Q.Test.make ~count ~name:"filter_mapi"
    Q.Gen.(triple (opt gen_path) (Q.fun2 obs_path Q.Observable.int (opt int)) gen_list)
    ~print:Q.Print.(triple (option print_path) Q.Fn.print print_list)
    (fun (rev_prefix, Fun (_, f), l) ->
       to_list
         (Trie.filter_mapi ?rev_prefix
            (fun ~rev_path -> f rev_path)
            (of_list l))
       =
       ListAsTrie.filter_mapi ?rev_prefix
         (fun ~rev_path -> f rev_path)
         l)

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
    Q.Gen.(triple gen_path (Q.fun1 Q.Observable.(option int) (opt int)) gen_list)
    ~print:Q.Print.(triple print_path Q.Fn.print print_list)
    (fun (p, Fun (_, f), l) ->
       to_list
         (Trie.update_singleton p f (of_list l))
       =
       ListAsTrie.update_singleton p f l)
let test_update_root =
  Q.Test.make ~count ~name:"update_root"
    Q.Gen.(pair (Q.fun1 Q.Observable.(option int) (opt int)) gen_list)
    ~print:Q.Print.(pair Q.Fn.print print_list)
    (fun (Fun (_, f), l) ->
       to_list
         (Trie.update_root f (of_list l))
       =
       ListAsTrie.update_root f l)

let test_union =
  Q.Test.make ~count ~name:"union"
    Q.Gen.(quad (opt gen_path) (Q.fun3 obs_path Q.Observable.int Q.Observable.int int) gen_list gen_list)
    ~print:Q.Print.(quad (option print_path) Q.Fn.print print_list print_list)
    (fun (rev_prefix, Fun (_, f), l1, l2) ->
       to_list
         (Trie.union ?rev_prefix
            (fun ~rev_path -> f rev_path)
            (of_list l1) (of_list l2))
       =
       ListAsTrie.union ?rev_prefix
         (fun ~rev_path -> f rev_path)
         l1 l2)

let test_union_subtree =
  Q.Test.make ~count ~name:"union_subtree"
    Q.Gen.(quad (opt gen_path) (Q.fun3 obs_path Q.Observable.int Q.Observable.int int) gen_list (pair gen_path gen_list))
    ~print:Q.Print.(quad (option print_path) Q.Fn.print print_list (pair print_path print_list))
    (fun (rev_prefix, Fun (_, f), l1, (pre, l2)) ->
       to_list
         (Trie.union_subtree ?rev_prefix
            (fun ~rev_path -> f rev_path)
            (of_list l1) (pre, of_list l2))
       =
       ListAsTrie.union_subtree ?rev_prefix
         (fun ~rev_path -> f rev_path)
         l1 (pre, l2))

let test_union_singleton =
  Q.Test.make ~count ~name:"union_singleton"
    Q.Gen.(quad (opt gen_path) (Q.fun3 obs_path Q.Observable.int Q.Observable.int int) gen_list (pair gen_path int))
    ~print:Q.Print.(quad (option print_path) Q.Fn.print print_list (pair print_path int))
    (fun (rev_prefix, Fun (_, f), l1, b) ->
       to_list
         (Trie.union_singleton ?rev_prefix
            (fun ~rev_path -> f rev_path)
            (of_list l1) b)
       =
       ListAsTrie.union_singleton ?rev_prefix
         (fun ~rev_path -> f rev_path)
         l1 b)

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

let test_to_seq =
  Q.Test.make ~count ~name:"to_seq" Q.Gen.(pair (opt gen_path) gen_list)
    ~print:Q.Print.(pair (option print_path) print_list)
    (fun (rev_prefix, l) ->
       List.of_seq (Trie.to_seq ?rev_prefix (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq ?rev_prefix l))

let test_to_seq_with_reversed_paths =
  Q.Test.make ~count ~name:"to_seq_with_reversed_paths" Q.Gen.(pair (opt gen_path) gen_list)
    ~print:Q.Print.(pair (option print_path) print_list)
    (fun (rev_prefix, l) ->
       List.of_seq (Trie.to_seq_with_reversed_paths ?rev_prefix (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq_with_reversed_paths ?rev_prefix l))

let test_to_seq_values =
  Q.Test.make ~count ~name:"to_seq_values" gen_list ~print:print_list
    (fun l ->
       List.of_seq (Trie.to_seq_values (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq_values l))

let test_of_seq =
  Q.Test.make ~count ~name:"of_seq"
    Q.Gen.(triple (opt gen_path) (Q.fun3 obs_path Q.Observable.int Q.Observable.int int) (small_list @@ pair gen_path int))
    ~print:Q.Print.(triple (option print_path) Q.Fn.print (list @@ pair print_path int))
    (fun (rev_prefix, Fun (_, f), l) ->
       to_list (Trie.of_seq ?rev_prefix (fun ~rev_path -> f rev_path) (List.to_seq l))
       =
       ListAsTrie.of_seq ?rev_prefix (fun ~rev_path -> f rev_path) (List.to_seq l))

module Result =
struct

  let gen_result gen : ('a, int) result Q.Gen.t =
    let open Q.Gen in
    bind (int_range 0 19) @@ fun n ->
    if n < 19
    then map Result.ok gen
    else map Result.error small_int

  let test_update_subtree =
    Q.Test.make ~count ~name:"update_subtree"
      Q.Gen.(triple gen_path (Q.fun1 obs_list (gen_result gen_list)) gen_list)
      ~print:Q.Print.(triple print_path Q.Fn.print print_list)
      (fun (p, Q.Fun (_, f), l) ->
         Result.map to_list
           (Trie.Result.update_subtree p (fun t -> Result.map of_list @@ f (to_list t)) (of_list l))
         =
         ListAsTrie.Result.update_subtree p f l)

  let test_update_singleton =
    Q.Test.make ~count ~name:"update_singleton"
      Q.Gen.(triple gen_path (Q.fun1 Q.Observable.(option int) (gen_result @@ opt int)) gen_list)
      ~print:Q.Print.(triple print_path Q.Fn.print print_list)
      (fun (p, Q.Fun (_, f), l) ->
         Result.map to_list
           (Trie.Result.update_singleton p f (of_list l))
         =
         ListAsTrie.Result.update_singleton p f l)

  let test_update_root =
    Q.Test.make ~count ~name:"update_root"
      Q.Gen.(pair (Q.fun1 Q.Observable.(option int) (gen_result @@ opt int)) gen_list)
      ~print:Q.Print.(pair Q.Fn.print print_list)
      (fun (Q.Fun (_, f), l) ->
         Result.map to_list
           (Trie.Result.update_root f (of_list l))
         =
         ListAsTrie.Result.update_root f l)

  let test_union =
    Q.Test.make ~count ~name:"update_union"
      Q.Gen.(quad (opt gen_path) (Q.fun3 obs_path Q.Observable.int Q.Observable.int (gen_result int)) gen_list gen_list)
      ~print:Q.Print.(quad (option print_path) Q.Fn.print print_list print_list)
      (fun (rev_prefix, Q.Fun (_, f), l1, l2) ->
         Result.map to_list
           (Trie.Result.union ?rev_prefix (fun ~rev_path -> f rev_path) (of_list l1) (of_list l2))
         =
         ListAsTrie.Result.union ?rev_prefix (fun ~rev_path -> f rev_path) l1 l2)

  let test_union_subtree =
    Q.Test.make ~count ~name:"update_union_subtree"
      Q.Gen.(quad (opt gen_path) (Q.fun3 obs_path Q.Observable.int Q.Observable.int (gen_result int)) gen_list (pair gen_path gen_list))
      ~print:Q.Print.(quad (option print_path) Q.Fn.print print_list (pair print_path print_list))
      (fun (rev_prefix, Q.Fun (_, f), l1, (pre, l2)) ->
         Result.map to_list
           (Trie.Result.union_subtree ?rev_prefix (fun ~rev_path -> f rev_path) (of_list l1) (pre, of_list l2))
         =
         ListAsTrie.Result.union_subtree ?rev_prefix (fun ~rev_path -> f rev_path) l1 (pre, l2))

  let test_union_singleton =
    Q.Test.make ~count ~name:"update_union_singleton"
      Q.Gen.(quad (opt gen_path) (Q.fun3 obs_path Q.Observable.int Q.Observable.int (gen_result int)) gen_list (pair gen_path int))
      ~print:Q.Print.(quad (option print_path) Q.Fn.print print_list (pair print_path int))
      (fun (rev_prefix, Q.Fun (_, f), l1, b2) ->
         Result.map to_list
           (Trie.Result.union_singleton ?rev_prefix (fun ~rev_path -> f rev_path) (of_list l1) b2)
         =
         ListAsTrie.Result.union_singleton ?rev_prefix (fun ~rev_path -> f rev_path) l1 b2)
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
    ; test_iteri
    ; test_mapi
    ; test_filteri
    ; test_filter_mapi
    ; test_update_subtree
    ; test_update_singleton
    ; test_update_root
    ; test_union
    ; test_union_subtree
    ; test_union_singleton
    ; test_detach_subtree
    ; test_detach_singleton
    ; test_to_seq
    ; test_to_seq_with_reversed_paths
    ; test_to_seq_values
    ; test_of_seq
    ; Result.test_update_subtree
    ; Result.test_update_singleton
    ; Result.test_update_root
    ; Result.test_union
    ; Result.test_union_subtree
    ; Result.test_union_singleton
    ]
