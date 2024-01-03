type t = { counter : int Atomic.t; total : int }

let make total =
  { counter = Atomic.make 0 |> Multicore_magic.copy_as_padded; total }
  |> Multicore_magic.copy_as_padded

let await { counter; total } =
  if Atomic.get counter = total then
    Atomic.compare_and_set counter total 0 |> ignore;
  Atomic.incr counter;
  while Atomic.get counter < total do
    Domain.cpu_relax ()
  done
