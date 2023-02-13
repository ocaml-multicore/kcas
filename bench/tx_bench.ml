module Loc = Kcas.Loc
module Tx = Kcas.Tx

let make_kCAS k =
  let rs = List.init k (fun _ -> Loc.make 0) in
  let rec incs = function
    | [] -> failwith "bug"
    | [ r ] -> Tx.(modify r (( + ) 1))
    | r :: rs -> Tx.(modify r (( + ) 1) >> incs rs)
  in
  let rec decs = function
    | [] -> failwith "bug"
    | [ r ] -> Tx.(modify r (( + ) (-1)))
    | r :: rs -> Tx.(modify r (( + ) (-1)) >> decs rs)
  in
  (incs rs, decs rs)

let benchmark ~num_iter ~num_ops () =
  let operation1, operation2 = make_kCAS num_ops in
  let rec loop i =
    if i > 0 then (
      ignore @@ Tx.commit operation1;
      ignore @@ Tx.commit operation2;
      loop (i - 1))
  in
  loop num_iter

let run ~num_iter ~num_ops () =
  let n = 10 in
  Benchmark.run (benchmark ~num_iter ~num_ops) n
