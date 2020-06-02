let rec map f =
  function
  | [] -> Ok []
  | x :: xs ->
    Result.bind (f x) @@ fun x ->
    Result.bind (map f xs) @@ fun xs ->
    Ok (x :: xs)
