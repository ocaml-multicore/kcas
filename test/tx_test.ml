open Kcas

module Q = struct
  include Kcas_data.Queue

  let is_empty = Tx.is_empty
  let push_back = Tx.add

  let pop_front q =
    Tx.take_opt q |> Kcas.Tx.map @@ function None -> raise Exit | Some x -> x
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
       Q.push_back x q));

  assert (Tx.commit (Q.pop_front q) = 4);
  assert (Tx.commit (Q.is_empty q));

  (* Transfer item from queue [p] to stack [s] atomically *)
  Tx.(commit (P.pop_front p >>= S.push s));

  assert (Tx.commit (S.pop s) = 1);
  assert (Tx.commit (P.pop_front p) = 3);
  assert (Tx.commit (P.is_empty p));

  Tx.commit (Q.push_back 101 q);
  assert (not (Tx.commit (Q.is_empty q)))
