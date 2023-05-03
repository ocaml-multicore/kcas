open Kcas
open Kcas_data

let () =
  let q = Queue.create () in
  Queue.add 101 q;
  let tx ~xt =
    Queue.Xt.add ~xt 42 q;
    assert (Queue.Xt.take_opt ~xt q = Some 101);
    assert (Queue.Xt.length ~xt q = 1);
    assert (Queue.Xt.take_opt ~xt q = Some 42);
    assert (Queue.Xt.take_opt ~xt q = None)
  in
  Xt.commit { tx }

let () =
  let q = Queue.create () in
  assert (Queue.length q = 0);
  assert (Queue.is_empty q);
  Queue.add 101 q;
  assert (Queue.length q = 1);
  assert (not (Queue.is_empty q));
  let r = Queue.copy q in
  assert (Queue.peek_opt q = Some 101);
  Queue.add 42 q;
  assert (List.of_seq (Queue.to_seq q) = [ 101; 42 ]);
  Queue.swap q r;
  assert (Queue.peek_opt q = Some 101);
  assert (Queue.take_opt q = Some 101);
  assert (Queue.take_opt q = None);
  assert (Queue.take_opt r = Some 101);
  assert (Queue.take_opt r = Some 42);
  assert (Queue.take_opt r = None);

  Printf.printf "Test Queue OK!\n%!"
