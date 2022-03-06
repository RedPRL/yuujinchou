open Seq
open ResultMonad.Syntax

let rec mmerge ~f s1 s2 : ((string * 'a) Seq.t, 'b) result =
  match s1 (), s2 () with
  | Nil, Nil -> ret Seq.empty
  | Nil, n2 -> ret @@ fun () -> n2
  | n1, Nil -> ret @@ fun () -> n1
  | Cons ((k1, v1), s1'), Cons ((k2, v2), s2') ->
    let c = String.compare k1 k2 in
    if c = 0 then
      let* v = f k1 v1 v2 in
      match v with
      | None -> mmerge ~f s1' s2'
      | Some v ->
        let* s = mmerge ~f s1' s2' in
        ret @@ fun () -> Cons ((k1, v), s)
    else if c < 0 then
      let* s = mmerge ~f s1' s2 in
      ret @@ fun () -> Cons ((k1, v1), s)
    else
      let* s = mmerge ~f s1 s2' in
      ret @@ fun () -> Cons ((k2, v2), s)
