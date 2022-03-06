module Syntax =
struct
  let ret = Result.ok
  let error = Result.error
  let (>>=) = Result.bind
  let (<$>) = Result.map
  let (let*) = Result.bind
  let[@inline] (and*) m n = let* m = m in let* n = n in ret (m, n)
  let[@inline] (let+) m f = Result.map f m
  let (and+) = (and*)
end

open Syntax

let rec map ~f =
  function
  | [] -> ret []
  | x :: xs ->
    let+ y = f x
    and+ ys = map ~f xs in
    y :: ys

let rec iter ~f =
  function
  | [] -> ret ()
  | x :: xs ->
    let* () = f x in
    iter ~f xs

let rec fold_left ~f ~init =
  function
  | [] -> ret init
  | x :: xs ->
    let* init = f init x in
    fold_left ~f ~init xs
