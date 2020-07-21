module Syntax =
struct
  let ret x = Result.ok x
  let fail e = Result.error e
  let (let*) = Result.bind
  let (and*) ma mb = let* a = ma in let* b = mb in ret (a, b)
  let (let+) ma f = Result.map f ma
  let (and+) = (and*)
end

open Syntax

let rec map f =
  function
  | [] -> ret []
  | x :: xs ->
    let+ y = f x
    and+ ys = map f xs in
    y :: ys

let rec iter f =
  function
  | [] -> ret ()
  | x :: xs ->
    let* () = f x in
    let+ () = iter f xs in
    ()
