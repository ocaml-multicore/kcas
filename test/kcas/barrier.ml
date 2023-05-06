type t = { counter : int Atomic.t; total : int }

let make total = { counter = Atomic.make 0; total }

let await { counter; total } =
  Atomic.incr counter;
  while Atomic.get counter < total do
    ()
  done
