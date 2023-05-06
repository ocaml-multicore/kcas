open Kcas

module Q = struct
  type 'a node = Nil | Node of 'a * 'a node Loc.t
  type 'a queue = { head : 'a node Loc.t Loc.t; tail : 'a node Loc.t Atomic.t }

  let queue () =
    let next = Loc.make Nil in
    { head = Loc.make next; tail = Atomic.make next }

  let try_dequeue ~xt { head; _ } =
    let old_head = Xt.get ~xt head in
    match Xt.get ~xt old_head with
    | Nil -> None
    | Node (value, next) ->
        Xt.set ~xt head next;
        Some value

  let enqueue ~xt { tail; _ } value =
    let new_tail = Loc.make Nil in
    let new_node = Node (value, new_tail) in
    let rec find_and_set_tail old_tail =
      match Xt.compare_and_swap ~xt old_tail Nil new_node with
      | Nil -> ()
      | Node (_, old_tail) -> find_and_set_tail old_tail
    in
    find_and_set_tail (Atomic.get tail);
    let rec fix_tail () =
      let old_tail = Atomic.get tail in
      if
        Loc.get new_tail == Nil
        && not (Atomic.compare_and_set tail old_tail new_tail)
      then fix_tail ()
    in
    Xt.post_commit ~xt fix_tail

  let check_tail { tail; _ } = Loc.get (Atomic.get tail) == Nil
end

let failure exit msg =
  Atomic.set exit true;
  Printf.printf "%s\n%!" msg;
  failwith msg

let write_skew_test n =
  let q1 = Q.queue () and q2 = Q.queue () in

  let push_to_q2 ~xt =
    match Q.try_dequeue ~xt q1 with None -> Q.enqueue ~xt q2 42 | Some _ -> ()
  and push_to_q1 ~xt =
    match Q.try_dequeue ~xt q2 with None -> Q.enqueue ~xt q1 24 | Some _ -> ()
  and clear ~xt = (Q.try_dequeue ~xt q1, Q.try_dequeue ~xt q2) in

  let barrier = Atomic.make 3 in
  let sync () =
    Atomic.decr barrier;
    while Atomic.get barrier != 0 do
      Domain.cpu_relax ()
    done
  in

  let exit = Atomic.make false in

  let domains =
    [
      Domain.spawn (fun () ->
          sync ();
          while not (Atomic.get exit) do
            Xt.commit { tx = push_to_q1 }
          done);
      Domain.spawn (fun () ->
          sync ();
          while not (Atomic.get exit) do
            Xt.commit { tx = push_to_q2 }
          done);
    ]
  in

  sync ();
  for _ = 1 to n do
    match Xt.commit { tx = clear } with
    | Some _, Some _ -> failure exit "write skew!"
    | _ -> ()
  done;
  Atomic.set exit true;

  List.iter Domain.join domains

let tail_leak_test n =
  let q = Q.queue () in

  let m = 2 in

  let exit = Atomic.make false
  and rounds = Array.init m @@ fun _ -> Atomic.make (n * 2) in
  let finished () =
    Array.exists (fun round -> Atomic.get round <= 0) rounds || Atomic.get exit
  and sync i =
    let n = Atomic.fetch_and_add rounds.(i) (-1) - 1 in
    while rounds |> Array.exists @@ fun round -> n < Atomic.get round do
      if Atomic.get exit then failwith "exit"
    done
  in

  let domain i () =
    try
      while not (finished ()) do
        sync i;

        Xt.commit { tx = Q.enqueue q 42 };
        if None == Xt.commit { tx = Q.try_dequeue q } then
          failure exit "impossible!";

        sync i;

        if not (Q.check_tail q) then failure exit "tail leak!"
      done
    with e ->
      Atomic.set exit true;
      raise e
  in

  List.init m domain |> List.map Domain.spawn |> List.iter Domain.join

let () =
  let n = try int_of_string Sys.argv.(1) with _ -> 1_000 in
  write_skew_test n;
  tail_leak_test n;
  Printf.printf "Test MS queue OK!\n%!"
