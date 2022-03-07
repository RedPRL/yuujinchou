open StdLabels
open Yuujinchou
module LT = ListAsTrie
module Q = QCheck2

let cmp_path = List.compare ~cmp:String.compare
let cmp (p1, _) (p2, _) = cmp_path p1 p2

type data = N of int | M of data * data | P of LT.path * data
let gen_data = Q.Gen.(map (fun i -> N i) int)
let seq_to_list s = ListAsTrie.of_seq (fun ~rev_path:_ _ _ -> failwith "Conflicting bindings") s
let rawlist_to_list l = seq_to_list @@ List.to_seq l
let list_to_rawlist l = List.of_seq @@ ListAsTrie.to_seq l

let print_path = Q.Print.(list string)
let rec print_data = function
  | N i -> Format.sprintf "N %i" i
  | M (d1, d2) -> Format.sprintf "M (%s, %s)" (print_data d1) (print_data d2)
  | P (p, d) -> Format.sprintf "P (%s, %s)" (print_path p) (print_data d)
and print_list l = Q.Print.(contramap list_to_rawlist (list @@ pair print_path print_data)) l
let print_list' p = Q.Print.(contramap list_to_rawlist (list @@ pair print_path (pair p print_data)))

let gen_path = Q.Gen.(small_list @@ small_string ~gen:printable)
let gen_list = Q.Gen.map (fun l -> rawlist_to_list @@ List.sort_uniq ~cmp l)
    Q.Gen.(small_list @@ pair gen_path gen_data)
let gen_list' g = Q.Gen.map (fun l -> rawlist_to_list @@ List.sort_uniq ~cmp l)
    Q.Gen.(small_list @@ pair gen_path (pair g gen_data))
let of_list l = Trie.of_seq (fun ~rev_path:_ _ _ -> failwith "Conflicting bindings") @@ ListAsTrie.to_seq l
let to_list t = seq_to_list @@ Trie.to_seq t

let bag_create () = ref []
let bag_push x l = l := x :: !l
let bag_eq l1 l2 = List.stable_sort ~cmp:compare l1 = List.stable_sort ~cmp:compare l2

let test_empty =
  Q.Test.make ~count:100 ~name:"empty" Q.Gen.unit
    (fun () -> to_list Trie.empty = ListAsTrie.empty)
let test_is_empty =
  Q.Test.make ~count:100 ~name:"is_empty" gen_list ~print:print_list
    (fun l -> Trie.is_empty (of_list l) = ListAsTrie.is_empty l)
let test_root =
  Q.Test.make ~count:100 ~name:"root" gen_data ~print:print_data
    (fun x -> to_list (Trie.root x) = ListAsTrie.root x)
let test_root_opt =
  Q.Test.make ~count:100 ~name:"root_opt" Q.Gen.(opt gen_data) ~print:Q.Print.(option print_data)
    (fun x -> to_list (Trie.root_opt x) = ListAsTrie.root_opt x)
let test_prefix =
  Q.Test.make ~count:100 ~name:"prefix" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> to_list (Trie.prefix p (of_list l)) = ListAsTrie.prefix p l)
let test_singleton =
  Q.Test.make ~count:100 ~name:"singleton" Q.Gen.(pair gen_path gen_data) ~print:Q.Print.(pair print_path print_data)
    (fun (p, x) -> to_list (Trie.singleton (p, x)) = ListAsTrie.singleton (p, x))
let test_find_subtree =
  Q.Test.make ~count:100 ~name:"find_subtree" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> to_list (Trie.find_subtree p (of_list l)) = ListAsTrie.find_subtree p l)
let test_find_singleton =
  Q.Test.make ~count:100 ~name:"find_singleton" Q.Gen.(pair gen_path gen_list) ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) -> Trie.find_singleton p (of_list l) = ListAsTrie.find_singleton p l)
let test_find_root =
  Q.Test.make ~count:100 ~name:"find_root" gen_list ~print:print_list
    (fun l -> Trie.find_root (of_list l) = ListAsTrie.find_root l)
let test_iteri =
  Q.Test.make ~count:100 ~name:"iteri" Q.Gen.(pair (opt gen_path) gen_list) ~print:Q.Print.(pair (option print_path) print_list)
    (fun (rev_prefix, l) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       Trie.iteri ?rev_prefix (fun ~rev_path x -> bag_push (rev_path, x) calls1) (of_list l);
       ListAsTrie.iteri ?rev_prefix (fun ~rev_path x -> bag_push (rev_path, x) calls2) l;
       !calls1 = !calls2)
let test_mapi =
  Q.Test.make ~count:100 ~name:"mapi" Q.Gen.(pair (opt gen_path) gen_list) ~print:Q.Print.(pair (option print_path) print_list)
    (fun (rev_prefix, l) ->
       to_list (Trie.mapi ?rev_prefix (fun ~rev_path x -> P (rev_path, x)) (of_list l))
       =
       ListAsTrie.mapi ?rev_prefix (fun ~rev_path x -> P (rev_path, x)) l;)
let test_filteri =
  Q.Test.make ~count:100 ~name:"filteri" Q.Gen.(pair (opt gen_path) (gen_list' bool))
    ~print:Q.Print.(pair (option print_path) (print_list' bool))
    (fun (rev_prefix, l) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       to_list (Trie.filteri ?rev_prefix (fun ~rev_path (b, x) -> bag_push (rev_path, b, x) calls1; b) (of_list l))
       =
       ListAsTrie.filteri ?rev_prefix (fun ~rev_path (b, x) -> bag_push (rev_path, b, x) calls2; b) l
       &&
       bag_eq !calls1 !calls2)
let test_filter_mapi =
  Q.Test.make ~count:100 ~name:"filter_mapi" Q.Gen.(pair (opt gen_path) (gen_list' bool))
    ~print:Q.Print.(pair (option print_path) (print_list' bool))
    (fun (rev_prefix, l) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       to_list
         (Trie.filter_mapi ?rev_prefix
            (fun ~rev_path (b, x) ->
               bag_push (rev_path, b, x) calls1;
               if b then None else Some (P (rev_path, x)))
            (of_list l))
       =
       ListAsTrie.filter_mapi ?rev_prefix
         (fun ~rev_path (b, x) ->
            bag_push (rev_path, b, x) calls2;
            if b then None else Some (P (rev_path, x)))
         l
       &&
       bag_eq !calls1 !calls2)

let test_update_subtree =
  Q.Test.make ~count:100 ~name:"update_subtree" Q.Gen.(triple gen_path gen_list gen_list)
    ~print:Q.Print.(triple print_path print_list print_list)
    (fun (p, l1, l2) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       to_list
         (Trie.update_subtree p
            (fun t -> bag_push (to_list t) calls1; of_list l2)
            (of_list l1))
       =
       ListAsTrie.update_subtree p
         (fun t -> bag_push t calls2; l2)
         l1
       &&
       bag_eq !calls1 !calls2)
let test_update_singleton =
  Q.Test.make ~count:100 ~name:"update_singleton" Q.Gen.(triple gen_path (opt gen_data) gen_list)
    ~print:Q.Print.(triple print_path (option print_data) print_list)
    (fun (p, new_x, l) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       to_list
         (Trie.update_singleton p
            (fun x -> bag_push x calls1; new_x)
            (of_list l))
       =
       ListAsTrie.update_singleton p
         (fun x -> bag_push x calls2; new_x)
         l
       &&
       bag_eq !calls1 !calls2)
let test_update_root =
  Q.Test.make ~count:100 ~name:"update_root" Q.Gen.(pair (opt gen_data) gen_list)
    ~print:Q.Print.(pair (option print_data) print_list)
    (fun (new_x, l) ->
       let calls1 = bag_create () in
       let calls2 = bag_create () in
       to_list
         (Trie.update_root
            (fun x -> bag_push x calls1; new_x)
            (of_list l))
       =
       ListAsTrie.update_root
         (fun x -> bag_push x calls2; new_x)
         l
       &&
       bag_eq !calls1 !calls2)

let test_union =
  Q.Test.make ~count:100 ~name:"union" Q.Gen.(triple (opt gen_path) gen_list gen_list)
    ~print:Q.Print.(triple (option print_path) print_list print_list)
    (fun (rev_prefix, l1, l2) ->
       to_list
         (Trie.union ?rev_prefix
            (fun ~rev_path x y -> P (rev_path, M (x, y)))
            (of_list l1) (of_list l2))
       =
       ListAsTrie.union ?rev_prefix
         (fun ~rev_path x y -> P (rev_path, M (x, y)))
         l1 l2)

let test_union_subtree =
  Q.Test.make ~count:100 ~name:"union_subtree" Q.Gen.(triple (opt gen_path) gen_list (pair gen_path gen_list))
    ~print:Q.Print.(triple (option print_path) print_list (pair print_path print_list))
    (fun (rev_prefix, l1, (pre, l2)) ->
       to_list
         (Trie.union_subtree ?rev_prefix
            (fun ~rev_path x y -> P (rev_path, M (x, y)))
            (of_list l1) (pre, of_list l2))
       =
       ListAsTrie.union_subtree ?rev_prefix
         (fun ~rev_path x y -> P (rev_path, M (x, y)))
         l1 (pre, l2))

let test_union_singleton =
  Q.Test.make ~count:100 ~name:"union_singleton" Q.Gen.(triple (opt gen_path) gen_list (pair gen_path gen_data))
    ~print:Q.Print.(triple (option print_path) print_list (pair print_path print_data))
    (fun (rev_prefix, l1, b) ->
       to_list
         (Trie.union_singleton ?rev_prefix
            (fun ~rev_path x y -> P (rev_path, M (x, y)))
            (of_list l1) b)
       =
       ListAsTrie.union_singleton ?rev_prefix
         (fun ~rev_path x y -> P (rev_path, M (x, y)))
         l1 b)

let test_detach_subtree =
  Q.Test.make ~count:100 ~name:"detach_subtree" Q.Gen.(pair gen_path gen_list)
    ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) ->
       (fun (t1, t2) -> to_list t1, to_list t2)
         (Trie.detach_subtree p (of_list l))
       =
       ListAsTrie.detach_subtree p l)

let test_detach_singleton =
  Q.Test.make ~count:100 ~name:"detach_singleton" Q.Gen.(pair gen_path gen_list)
    ~print:Q.Print.(pair print_path print_list)
    (fun (p, l) ->
       (fun (x, t) -> x, to_list t)
         (Trie.detach_singleton p (of_list l))
       =
       ListAsTrie.detach_singleton p l)

let test_to_seq =
  Q.Test.make ~count:100 ~name:"to_seq" Q.Gen.(pair (opt gen_path) gen_list)
    ~print:Q.Print.(pair (option print_path) print_list)
    (fun (rev_prefix, l) ->
       List.of_seq (Trie.to_seq ?rev_prefix (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq ?rev_prefix l))

let test_to_seq_with_reversed_paths =
  Q.Test.make ~count:100 ~name:"to_seq_with_reversed_paths" Q.Gen.(pair (opt gen_path) gen_list)
    ~print:Q.Print.(pair (option print_path) print_list)
    (fun (rev_prefix, l) ->
       List.of_seq (Trie.to_seq_with_reversed_paths ?rev_prefix (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq_with_reversed_paths ?rev_prefix l))

let test_to_seq_values =
  Q.Test.make ~count:100 ~name:"to_seq_values" gen_list ~print:print_list
    (fun l ->
       List.of_seq (Trie.to_seq_values (of_list l))
       =
       List.of_seq (ListAsTrie.to_seq_values l))

let test_of_seq =
  Q.Test.make ~count:100 ~name:"of_seq" Q.Gen.(pair (opt gen_path) (small_list @@ pair gen_path gen_data))
    ~print:Q.Print.(pair (option print_path) (list @@ pair print_path print_data))
    (fun (rev_prefix, l) ->
       to_list (Trie.of_seq ?rev_prefix (fun ~rev_path x y -> P (rev_path, M (x, y))) (List.to_seq l))
       =
       ListAsTrie.of_seq ?rev_prefix (fun ~rev_path x y -> P (rev_path, M (x, y))) (List.to_seq l))

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
    ]
