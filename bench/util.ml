let iter_factor =
  let factor b = if b then 10 else 1 in
  factor (64 <= Sys.word_size)
  * factor (Sys.backend_type = Native)
  * factor (1 < Domain.recommended_domain_count ())

let rec alloc ?(batch = 1000) counter =
  let n = Atomic.get counter in
  if n = 0 then 0
  else
    let batch = Int.min n batch in
    if Atomic.compare_and_set counter n (n - batch) then batch
    else alloc ~batch counter

let cross xs ys =
  xs |> List.concat_map @@ fun x -> ys |> List.map @@ fun y -> (x, y)
