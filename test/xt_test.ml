open Kcas

module Q = struct
  include Tx_linked_queue

  let is_empty ~xt q = Xt.of_tx ~xt @@ is_empty q
  let push_back ~xt q x = Xt.of_tx ~xt @@ push_back q x
  let push_front ~xt q x = Xt.of_tx ~xt @@ push_front q x

  let pop_front_opt ~xt q =
    Xt.of_tx ~xt Tx.(pop_front q |> map Option.some <|> return None)
end

module P = Xt_three_stack_queue
module S = Xt_stack

let () =
  let p = P.create () and q = Q.create () and s = S.create () in

  (* Populate [p] with two items atomically  *)
  let tx ~xt =
    P.push_front ~xt p 4;
    P.push_back ~xt p 1
  in
  Xt.commit { tx };

  Xt.commit { tx = P.push_back p 3 };

  assert (not (Xt.commit { tx = P.is_empty p }));

  (* Transfer item from [p] queue to [q] queue atomically *)
  let tx ~xt = P.pop_front_opt ~xt p |> Option.iter @@ Q.push_back ~xt q in
  Xt.commit { tx };

  assert (Xt.commit { tx = Q.pop_front_opt q } = Some 4);
  assert (Xt.commit { tx = Q.is_empty q });

  (* Transfer item from queue [p] to stack [s] atomically *)
  Xt.commit
    { tx = (fun ~xt -> P.pop_front_opt ~xt p |> Option.iter @@ S.push ~xt s) };

  assert (Xt.commit { tx = S.pop_opt s } = Some 1);
  assert (Xt.commit { tx = P.pop_front_opt p } = Some 3);
  assert (Xt.commit { tx = P.is_empty p });

  Xt.commit { tx = Q.push_front q 101 };
  assert (not (Xt.commit { tx = Q.is_empty q }))
