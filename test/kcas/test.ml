(*---------------------------------------------------------------------------
    Copyright (c) 2016 KC Sivaramakrishnan. All rights reserved.
    Distributed under the ISC license, see terms at the end of the file.
    %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*
########
  Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

(*---------------------------------------------------------------------------
    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted, provided that the above
    copyright notice and this permission notice appear in all copies.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

open Kcas

let nb_iter = 100_000

let assert_kcas loc expected_v =
  let present_v = Loc.get loc in
  assert (present_v == expected_v)

let run_domains = function
  | [] -> ()
  | main :: others ->
      let others = List.map Domain.spawn others in
      main ();
      List.iter Domain.join others

let test_non_linearizable () =
  let barrier = Barrier.make 2
  and n_iter = 100_000
  and test_finished = ref false in

  let a = Loc.make 0 and b = Loc.make 0 in

  let cass1a = [ Op.make_cmp b 0; Op.make_cas a 0 1 ]
  and cass1b = [ Op.make_cmp b 0; Op.make_cas a 1 0 ]
  and cass2a = [ Op.make_cas b 0 1; Op.make_cmp a 0 ]
  and cass2b = [ Op.make_cas b 1 0; Op.make_cmp a 0 ] in

  let atomically cs =
    if Random.bool () then
      try Op.atomically ~mode:Mode.obstruction_free cs
      with Mode.Interference -> false
    else Op.atomically cs
  in

  let thread1 () =
    Barrier.await barrier;
    while not !test_finished do
      if atomically cass1a then
        while not (atomically cass1b) do
          assert (Loc.get a == 1 && Loc.get b == 0)
        done
    done
  and thread2 () =
    Barrier.await barrier;
    for _ = 1 to n_iter do
      if atomically cass2a then
        while not (atomically cass2b) do
          assert (Loc.get a == 0 && Loc.get b == 1)
        done
    done;
    test_finished := true
  in

  run_domains [ thread2; thread1 ]

(* *)

let test_set () =
  let a = Loc.make 0 in
  assert_kcas a 0;
  Loc.set a 1;
  assert_kcas a 1

(* *)

let test_casn () =
  let barrier = Barrier.make 3 in
  let test_finished = Atomic.make false in

  let a1 = Loc.make 0 in
  let a2 = Loc.make 0 in

  let thread1 () =
    let c1 = [ Op.make_cas a1 0 1; Op.make_cas a2 0 1 ] in
    let c2 = [ Op.make_cas a1 1 0; Op.make_cas a2 1 0 ] in

    Barrier.await barrier;

    for _ = 1 to nb_iter do
      assert_kcas a1 0;
      assert_kcas a2 0;

      let out1 = Op.atomically c1 in
      assert out1;

      assert_kcas a1 1;
      assert_kcas a2 1;

      let out2 = Op.atomically c2 in
      assert out2
    done;
    Atomic.set test_finished true
  and thread2 () =
    let c1 = [ Op.make_cas a1 1 0; Op.make_cas a2 0 1 ] in
    let c2 = [ Op.make_cas a1 0 1; Op.make_cas a2 1 0 ] in

    Barrier.await barrier;

    while not (Atomic.get test_finished) do
      let out1 = Op.atomically c1 in
      let out2 = Op.atomically c2 in
      assert (not out1);
      assert (not out2)
    done
  and thread3 () =
    let c1 = [ Op.make_cas a1 0 1; Op.make_cas a2 1 0 ] in
    let c2 = [ Op.make_cas a1 1 0; Op.make_cas a2 0 1 ] in

    Barrier.await barrier;

    while not (Atomic.get test_finished) do
      let out1 = Op.atomically c1 in
      let out2 = Op.atomically c2 in
      assert (not out1);
      assert (not out2)
    done
  in

  run_domains [ thread1; thread2; thread3 ]

(* *)

let test_read_casn () =
  let barrier = Barrier.make 4 in
  let test_finished = Atomic.make false in

  let a1 = Loc.make 0 in
  let a2 = Loc.make 0 in

  let mutator () =
    Barrier.await barrier;
    for i = 0 to nb_iter do
      let c = [ Op.make_cas a1 i (i + 1); Op.make_cas a2 i (i + 1) ] in
      assert (Op.atomically c)
    done;
    Atomic.set test_finished true
  and getter () =
    Barrier.await barrier;
    while not (Atomic.get test_finished) do
      let a = Loc.get a1 in
      let b = Loc.get a2 in
      assert (a <= b)
    done
  and getaser () =
    Barrier.await barrier;
    while not (Atomic.get test_finished) do
      let a = Loc.get_as Fun.id a1 in
      let b = Loc.get_as Fun.id a2 in
      assert (a <= b)
    done
  and committer () =
    Barrier.await barrier;
    while not (Atomic.get test_finished) do
      let a = Xt.commit { tx = Xt.get a1 } in
      let b = Xt.commit { tx = Xt.get a2 } in
      assert (a <= b)
    done
  and updater () =
    Barrier.await barrier;
    while not (Atomic.get test_finished) do
      let a = Loc.update a1 Fun.id in
      let b = Loc.update a2 Fun.id in
      assert (a <= b)
    done
  in

  run_domains [ mutator; getter; getaser; committer; updater ]

(* *)

let test_stress n nb_loop =
  let make_loc n =
    let rec loop n out =
      if n > 0 then loop (n - 1) (Loc.make 0 :: out) else out
    in
    loop n []
  and make_kcas0 r_l =
    let rec loop r_l out =
      match r_l with h :: t -> loop t (Op.make_cas h 0 1 :: out) | [] -> out
    in
    loop r_l []
  and make_kcas1 r_l =
    let rec loop r_l out =
      match r_l with h :: t -> loop t (Op.make_cas h 1 0 :: out) | [] -> out
    in
    loop r_l []
  in
  let r_l = make_loc n in
  let kcas0 = make_kcas0 r_l in
  let kcas1 = make_kcas1 r_l in
  for _ = 1 to nb_loop do
    assert (Op.atomically kcas0);
    assert (Op.atomically kcas1)
  done

(* *)

(** Various tests make accesses in random order to exercise the internal splay
    tree based transaction log handling. *)
let in_place_shuffle array =
  let n = Array.length array in
  for i = 0 to n - 2 do
    let j = Random.int (n - i) + i in
    let t = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- t
  done

let test_presort () =
  let n_incs = 10_000 and n_domains = 3 and n_locs = 5 in

  let barrier = Barrier.make n_domains in

  let locs = Array.init n_locs (fun _ -> Loc.make 0) in

  let mk_inc locs =
    in_place_shuffle locs;
    let x = Loc.get locs.(0) in
    let y = x + 1 in
    Array.fold_left (fun cs r -> Op.make_cas r x y :: cs) [] locs
  in

  let thread () =
    let locs = Array.copy locs in
    Random.self_init ();
    Barrier.await barrier;
    for _ = 1 to n_incs do
      while not (Op.atomically (mk_inc locs)) do
        ()
      done
    done
  in

  run_domains (List.init n_domains (Fun.const thread));

  locs |> Array.iter (fun r -> assert (Loc.get r = n_incs * n_domains))

(* *)

let test_presort_and_is_in_log_xt () =
  let n_incs = 10_000 and n_domains = 3 and n_locs = 12 in
  let n_locs_half = n_locs asr 1 in

  let barrier = Barrier.make n_domains in

  let locs = Array.init n_locs (fun _ -> Loc.make 0) in

  let thread () =
    let locs = Array.copy locs in
    Random.self_init ();
    Barrier.await barrier;
    for _ = 1 to n_incs do
      in_place_shuffle locs;
      let tx ~xt =
        for i = 0 to n_locs_half - 1 do
          Xt.incr ~xt locs.(i)
        done;
        assert (Xt.is_in_log ~xt locs.(Random.int n_locs_half));
        assert (not (Xt.is_in_log ~xt locs.(n_locs_half)))
      in
      Xt.commit { tx }
    done
  in

  run_domains (List.init n_domains (Fun.const thread));

  let sum = locs |> Array.map Loc.get |> Array.fold_left ( + ) 0 in
  assert (sum = n_incs * n_locs_half * n_domains)

(* *)

let test_updates () =
  let x = Loc.make 0 in
  assert (Loc.fetch_and_add x 1 = 0);
  assert (Loc.get x = 1);
  Loc.incr x;
  assert (Loc.get x = 2);
  Loc.set x 1;
  assert (Loc.get x = 1);
  Loc.decr x;
  assert (Loc.get x = 0);
  assert (Loc.exchange x 1 = 0)

(* *)

let test_post_commit () =
  let attempt_with_post_commit ~expect { Xt.tx } =
    let count = ref 0 in
    let tx ~xt =
      tx ~xt;
      Xt.post_commit ~xt @@ fun () -> incr count
    in
    (try
       count := 0;
       Xt.commit ~mode:Mode.obstruction_free { tx }
     with Exit -> ());
    assert (!count = expect)
  in
  let tx ~xt:_ = raise Exit in
  attempt_with_post_commit ~expect:0 { tx };
  let tx ~xt:_ = () in
  attempt_with_post_commit ~expect:1 { tx };
  let a = Loc.make 0 and b = Loc.make 0 in
  attempt_with_post_commit ~expect:1 { tx = Xt.modify a Fun.id };
  attempt_with_post_commit ~expect:1 { tx = Xt.incr a };
  let tx ~xt = Xt.set ~xt a (Xt.exchange ~xt b 0) in
  attempt_with_post_commit ~expect:1 { tx }

(* *)

let test_backoff () =
  let b = Backoff.create ~lower_wait_log:5 ~upper_wait_log:6 () in
  assert (Backoff.get_wait_log b = 5);
  let b = Backoff.once b in
  assert (Backoff.get_wait_log b = 6);
  let b = Backoff.once b in
  assert (Backoff.get_wait_log b = 6);
  let b = Backoff.reset b in
  assert (Backoff.get_wait_log b = 5)

(* *)

let test_blocking () =
  let state = Loc.make `Spawned in
  let await state' =
    (* Intentionally test that [Xt.modify] allows retry. *)
    let tx ~xt =
      Xt.modify ~xt state @@ fun state ->
      Retry.unless (state == state');
      state
    in
    Xt.commit { tx }
  in

  let a = Loc.make 0 and bs = Array.init 10 @@ fun _ -> Loc.make 0 in

  let n = 10_000 in

  let num_attempts = ref 0 in

  let other_domain =
    Domain.spawn @@ fun () ->
    Loc.set state `Get_a_non_zero;
    Loc.get_as
      (fun a ->
        incr num_attempts;
        Retry.unless (a != 0))
      a;

    Loc.set state `Update_a_zero;
    Loc.modify a (fun a ->
        incr num_attempts;
        Retry.unless (a = 0);
        a + 1);

    Loc.set state `Set_1_b_to_0;
    let bs = Array.copy bs in
    for _ = 1 to n do
      (* We access in random order to exercise tx log waiters handling. *)
      in_place_shuffle bs;
      let tx ~xt =
        incr num_attempts;
        match
          bs
          |> Array.find_map @@ fun b ->
             if Xt.get ~xt b = 1 then Some b
             else if Loc.has_awaiters b (* There must be no leaked waiters... *)
             then (
               (* ...except if main domain just set the loc *)
               assert (Loc.get b = 1);
               Retry.later ())
             else None
        with
        | None -> Retry.later ()
        | Some b -> Xt.set ~xt b 0
      in
      Xt.commit { tx }
    done
  in

  await `Get_a_non_zero;
  Loc.set a 1;
  assert (!num_attempts <= 2 + 1 (* Need to account for race to next. *));

  await `Update_a_zero;
  Loc.set a 0;
  assert (!num_attempts <= 4 + 1 (* Need to account for race to next. *));

  await `Set_1_b_to_0;
  for _ = 1 to n do
    let i = Random.int (Array.length bs) in
    Loc.set bs.(i) 1;
    Loc.get_as (fun b -> Retry.unless (b = 0)) bs.(i)
  done;

  Domain.join other_domain;

  assert (!num_attempts <= 4 + (n * 2));
  for i = 0 to Array.length bs - 1 do
    assert (not (Loc.has_awaiters bs.(i)))
  done

let test_no_unnecessary_wakeups () =
  let continue = Loc.make false and tries = Atomic.make 0 in

  let other_domain =
    Domain.spawn @@ fun () ->
    continue
    |> Loc.get_as @@ fun s ->
       Atomic.incr tries;
       Retry.unless s
  in

  while not (Loc.has_awaiters continue) do
    Domain.cpu_relax ()
  done;

  assert (Loc.compare_and_set continue false false);
  assert (not (Loc.update continue Fun.id));
  Loc.set continue false;

  Unix.sleepf 0.01;

  assert (Loc.has_awaiters continue && Atomic.get tries = 1);
  Loc.set continue true;
  Domain.join other_domain;
  assert ((not (Loc.has_awaiters continue)) && Atomic.get tries = 2)

(* *)

let test_periodic_validation () =
  let a = Loc.make 0 and b = Loc.make 0 and looping = ref false in
  let non_zero_difference_domain =
    Domain.spawn @@ fun () ->
    let rec tx ~xt =
      let d = Xt.get ~xt a - Xt.get ~xt b in
      if d <> 0 then d
      else (
        (* We explicitly want this tx to go into infinite loop!  Without
           validation this would never finish. *)
        looping := true;
        tx ~xt)
    in
    Xt.commit { tx }
  in

  while not !looping do
    Domain.cpu_relax ()
  done;

  Loc.set a 1;

  assert (1 = Domain.join non_zero_difference_domain)

(* *)

let test_explicit_validation () =
  let a = Loc.make 0 and b = Loc.make 0 in

  let exit = ref false and mutator_running = ref false in
  let mutator_domain =
    Domain.spawn @@ fun () ->
    mutator_running := true;
    while not !exit do
      let tx ~xt =
        Xt.decr ~xt a;
        Xt.incr ~xt b
      in
      Xt.commit { tx };
      Domain.cpu_relax ()
    done
  in

  let n = 100 in

  while not !mutator_running do
    Domain.cpu_relax ()
  done;

  for _ = 1 to n do
    let tx ~xt =
      let a' = Xt.get ~xt a and b' = Xt.get ~xt b in
      Xt.validate ~xt a;
      assert (a' + b' = 0)
    in
    Xt.commit { tx }
  done;

  exit := true;

  Domain.join mutator_domain

(* *)

let test_rollback () =
  let n_iter = 1_000 in

  let n_locs = 20 in

  let locs = Loc.make_array n_locs 0 in

  let accum = ref 0 in

  for _ = 1 to n_iter do
    let n_permanent = Random.int n_locs in
    let n_rollbacks = Random.int n_locs in

    let expected = ref false in
    let unexpected = ref false in

    let tx ~xt =
      in_place_shuffle locs;
      for i = 0 to n_permanent - 1 do
        Xt.incr ~xt locs.(i)
      done;
      Xt.post_commit ~xt (fun () -> expected := true);

      let snap = Xt.snapshot ~xt in
      in_place_shuffle locs;
      for i = 0 to n_rollbacks - 1 do
        Xt.incr ~xt locs.(i)
      done;
      Xt.post_commit ~xt (fun () -> unexpected := true);
      Xt.rollback ~xt snap
    in
    Xt.commit { tx };

    assert !expected;
    assert (not !unexpected);

    accum := n_permanent + !accum
  done;

  let sum = Array.map Loc.get locs |> Array.fold_left ( + ) 0 in
  assert (!accum = sum)

(* *)

let test_call () =
  let never = Xt.{ tx = (fun ~xt:_ -> Retry.later ()) } in
  let result =
    Xt.commit { tx = Xt.first [ Xt.call never; (fun ~xt:_ -> 101) ] }
  in
  assert (result = 101)

(* *)

(** This is a non-deterministic test that might fail occasionally. *)
let test_timeout () =
  Domain_local_timeout.set_system (module Thread) (module Unix);

  let check (op : ?timeoutf:float -> bool Loc.t -> unit) () =
    let x = Loc.make false in
    let finally =
      Domain_local_timeout.set_timeoutf 0.3 @@ fun () -> Loc.set x true
    in
    Fun.protect ~finally @@ fun () ->
    (match op ~timeoutf:0.1 x with
    | () -> assert false
    | exception Timeout.Timeout -> ());
    op ~timeoutf:0.5 x
  in
  run_domains
    [
      check (fun ?timeoutf x ->
          Loc.get_as ?timeoutf (fun x -> if x then () else Retry.later ()) x);
      check (fun ?timeoutf x ->
          Loc.update ?timeoutf x (fun x -> x || Retry.later ()) |> ignore);
      check (fun ?timeoutf x ->
          Loc.modify ?timeoutf x (fun x -> x || Retry.later ()));
      check (fun ?timeoutf x ->
          let y = Loc.make false in
          let tx ~xt =
            if not (Xt.get ~xt x) then Retry.later ();
            Xt.swap ~xt x y
          in
          Xt.commit ?timeoutf { tx });
      check (fun ?timeoutf x ->
          let y = Loc.make false in
          let tx ~xt =
            if not (Xt.get ~xt x) then Retry.invalid ();
            Xt.swap ~xt x y
          in
          Xt.commit ?timeoutf { tx });
    ]

(* *)

let test_mode () =
  assert (Loc.get_mode (Loc.make ~mode:Mode.lock_free 0) == Mode.lock_free);
  assert (
    Loc.get_mode (Loc.make ~mode:Mode.obstruction_free 0)
    == Mode.obstruction_free);
  assert (Loc.get_mode (Loc.make 0) == Mode.obstruction_free)

(* *)

type _ _loc_is_injective =
  | Int : int _loc_is_injective
  | Loc : 'a _loc_is_injective -> 'a Loc.t _loc_is_injective

(* *)

let test_xt () =
  let rx = Loc.make 0 in
  let ry = Loc.make 1 in
  let tx ~xt =
    let y = Xt.get ~xt ry in
    Xt.set ~xt rx y;
    let x' = Xt.get ~xt rx in
    assert (x' = y)
  in
  Xt.commit { tx };
  assert (Loc.get rx = Loc.get ry)

let () =
  Alcotest.run "kcas"
    [
      ( "non linearizable",
        [ Alcotest.test_case "" `Quick test_non_linearizable ] );
      ("set", [ Alcotest.test_case "" `Quick test_set ]);
      ("casn", [ Alcotest.test_case "" `Quick test_casn ]);
      ("read casn", [ Alcotest.test_case "" `Quick test_read_casn ]);
      ( "stress",
        [ Alcotest.test_case "" `Quick (fun () -> test_stress 1_000 1_0) ] );
      ("presort", [ Alcotest.test_case "" `Quick test_presort ]);
      ( "is_in_log",
        [ Alcotest.test_case "" `Quick test_presort_and_is_in_log_xt ] );
      ("updates", [ Alcotest.test_case "" `Quick test_updates ]);
      ("post commit", [ Alcotest.test_case "" `Quick test_post_commit ]);
      ("backoff", [ Alcotest.test_case "" `Quick test_backoff ]);
      ("blocking", [ Alcotest.test_case "" `Quick test_blocking ]);
      ( "no unnecessary wakeups",
        [ Alcotest.test_case "" `Quick test_no_unnecessary_wakeups ] );
      ( "pediodic validation",
        [ Alcotest.test_case "" `Quick test_periodic_validation ] );
      ( "explicit validation",
        [ Alcotest.test_case "" `Quick test_explicit_validation ] );
      ("rollback", [ Alcotest.test_case "" `Quick test_rollback ]);
      ("call", [ Alcotest.test_case "" `Quick test_call ]);
      ("mode", [ Alcotest.test_case "" `Quick test_mode ]);
      ("xt", [ Alcotest.test_case "" `Quick test_xt ]);
      ( "timeout (non-deterministic)",
        [ Alcotest.test_case "" `Quick test_timeout ] );
    ]
