open Kcas
open Kcas_data

let capacity () =
  let s = Queue.create ~capacity:2 () in
  assert (Queue.try_add 101 s);
  assert (Queue.try_add 42 s);
  assert (not (Queue.try_add 15 s));
  let d = Domain.spawn @@ fun () -> Queue.add 5 s in
  assert (Queue.take s = 101);
  let tx ~xt = Retry.unless (Queue.Xt.take_blocking ~xt s = 42) in
  Xt.commit { tx };
  assert (Queue.take_blocking s = 5);
  Domain.join d

let basics () =
  let q = Queue.create () in
  Queue.add 101 q;
  let tx ~xt =
    Queue.Xt.add ~xt 42 q;
    assert (Queue.Xt.take_opt ~xt q = Some 101);
    assert (Queue.Xt.length ~xt q = 1);
    assert (Queue.Xt.take_opt ~xt q = Some 42);
    assert (Queue.Xt.take_opt ~xt q = None)
  in
  Xt.commit { tx };
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
  assert (Queue.take_opt r = None)

let () =
  Alcotest.run "Queue"
    [
      ("basics", [ Alcotest.test_case "" `Quick basics ]);
      ("capacity", [ Alcotest.test_case "" `Quick capacity ]);
    ]
