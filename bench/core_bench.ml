module Loc = Kcas.Loc
module Op = Kcas.Op

let make_kCAS k =
  let rec loop k out1 out2 =
    if k > 0 then
      let a = Loc.make 0 in
      loop (k - 1) (Op.make_cas a 0 1 :: out1) (Op.make_cas a 1 0 :: out2)
    else (out1, out2)
  in
  loop k [] []

let benchmark ~num_iter ~num_ops () =
  let operation1, operation2 = make_kCAS num_ops in 
  let rec loop i =
    if i > 0 then (
      ignore @@ Op.atomically operation1;
      ignore @@ Op.atomically operation2;
      loop (i - 1))
  in
  loop num_iter

let run ~num_iter ~num_ops () =
  let n = 10 in 
   Benchmark.run (benchmark ~num_iter ~num_ops) n 
