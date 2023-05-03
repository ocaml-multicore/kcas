open Kcas_data

let () =
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
  assert (Stack.pop_opt t = None);

  Printf.printf "Test Stack OK!\n%!"
