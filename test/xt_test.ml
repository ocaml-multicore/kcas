open Kcas
module Q = Xt_linked_queue
module P = Kcas_data.Queue
module S = Xt_stack

let () =
  let p = P.create () and q = Q.create () and s = S.create () in

  (* Populate [p] with two items atomically  *)
  let tx ~xt =
    P.Xt.add ~xt 4 p;
    P.Xt.add ~xt 1 p
  in
  Xt.commit { tx };

  Xt.commit { tx = P.Xt.add 3 p };

  assert (not (Xt.commit { tx = P.Xt.is_empty p }));

  (* Transfer item from [p] queue to [q] queue atomically *)
  let tx ~xt = P.Xt.take_opt ~xt p |> Option.iter @@ Q.push_back ~xt q in
  Xt.commit { tx };

  assert (Xt.commit { tx = Q.pop_front q } = Some 4);
  assert (Xt.commit { tx = Q.is_empty q });

  (* Transfer item from queue [p] to stack [s] atomically *)
  let tx ~xt = P.Xt.take_opt ~xt p |> Option.iter @@ fun x -> S.push ~xt s x in
  Xt.commit { tx };

  assert (Xt.commit { tx = S.pop_opt s } = Some 1);
  assert (Xt.commit { tx = P.Xt.take_opt p } = Some 3);
  assert (Xt.commit { tx = P.Xt.is_empty p });

  Xt.commit { tx = Q.push_front q 101 };
  assert (not (Xt.commit { tx = Q.is_empty q }));

  Printf.printf "Test Xt OK!\n%!"
