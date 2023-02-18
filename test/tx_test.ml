open Kcas

module Q = struct
  include Xt_three_stack_queue

  let is_empty q = Xt.to_tx { tx = is_empty q }
  let push_back q x = Xt.to_tx { tx = push_back q x }
  let push_front q x = Xt.to_tx { tx = push_front q x }

  let pop_front q =
    Xt.to_tx { tx = pop_front_opt q }
    |> Tx.map @@ function None -> raise Exit | Some x -> x
end

module P = Tx_linked_queue
module S = Tx_stack

let () =
  let p = P.create () and q = Q.create () and s = S.create () in

  (* Populate [p] with two items atomically  *)
  Tx.(commit (P.push_front p 4 >> P.push_back p 1));

  Tx.commit (P.push_back p 3);

  assert (not (Tx.commit (P.is_empty p)));

  (* Transfer item from queue [p] to queue [q] atomically *)
  Tx.(
    commit
      (let* x = Q.pop_front q <|> P.pop_front p in
       Q.push_back q x));

  assert (Tx.commit (Q.pop_front q) = 4);
  assert (Tx.commit (Q.is_empty q));

  (* Transfer item from queue [p] to stack [s] atomically *)
  Tx.(commit (P.pop_front p >>= S.push s));

  assert (Tx.commit (S.pop s) = 1);
  assert (Tx.commit (P.pop_front p) = 3);
  assert (Tx.commit (P.is_empty p));

  Tx.commit (Q.push_front q 101);
  assert (not (Tx.commit (Q.is_empty q)))
