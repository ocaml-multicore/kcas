open Kcas
open Kcas_data

let capacity () =
  let s = Stack.create ~capacity:2 () in
  assert (Stack.try_push 101 s);
  assert (Stack.try_push 42 s);
  assert (not (Stack.try_push 15 s));
  let d = Domain.spawn @@ fun () -> Stack.push 5 s in
  assert (Stack.pop s = 42);
  let tx ~xt = Retry.unless (Stack.Xt.pop_blocking ~xt s = 5) in
  Xt.commit { tx };
  assert (Stack.pop s = 101);
  Domain.join d

let basics () =
  let s = Stack.create () in
  assert (Stack.length s = 0);
  assert (Stack.is_empty s);
  Stack.push 101 s;
  assert (not (Stack.is_empty s));
  assert (Stack.top_opt s = Some 101);
  assert (Stack.length s = 1);
  let t = Stack.copy s in
  assert (Stack.pop_opt t = Some 101);
  Stack.push 42 s;
  Stack.swap s t;
  assert (Stack.pop_opt s = None);
  assert (List.of_seq (Stack.to_seq t) = [ 42; 101 ]);
  assert (Stack.top_opt t = Some 42);
  assert (Stack.length t = 2);
  assert (Stack.pop_opt t = Some 42);
  assert (Stack.pop_opt t = Some 101);
  assert (Stack.pop_opt t = None)

let () =
  Alcotest.run "Stack"
    [
      ("basics", [ Alcotest.test_case "" `Quick basics ]);
      ("capacity", [ Alcotest.test_case "" `Quick capacity ]);
    ]
