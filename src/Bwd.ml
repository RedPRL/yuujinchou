type 'a bwd = Nil | Snoc of 'a bwd * 'a

let rec (<<) xs ys =
  match ys with
  | [] -> xs
  | y :: ys -> Snoc (xs, y) << ys

let rec (>>) xs ys =
  match xs with
  | Nil -> ys
  | Snoc (xs, x) -> xs >> x :: ys
