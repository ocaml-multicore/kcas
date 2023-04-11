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

let test_non_linearizable () =
  let barrier = Barrier.make 2
  and n_iter = 1_000_000
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

  [ thread2; thread1 ] |> List.map Domain.spawn |> List.iter Domain.join

(* test 1 *)
let test_set () =
  let a = Loc.make 0 in
  assert_kcas a 0;
  Loc.set a 1;
  assert_kcas a 1

(* test 2 *)
let thread1 barrier test_finished (a1, a2) () =
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

let thread2 barrier test_finished (a1, a2) () =
  let c1 = [ Op.make_cas a1 1 0; Op.make_cas a2 0 1 ] in
  let c2 = [ Op.make_cas a1 0 1; Op.make_cas a2 1 0 ] in

  Barrier.await barrier;

  while not (Atomic.get test_finished) do
    let out1 = Op.atomically c1 in
    let out2 = Op.atomically c2 in
    assert (not out1);
    assert (not out2)
  done

let thread3 barrier test_finished (a1, a2) () =
  let c1 = [ Op.make_cas a1 0 1; Op.make_cas a2 1 0 ] in
  let c2 = [ Op.make_cas a1 1 0; Op.make_cas a2 0 1 ] in

  Barrier.await barrier;

  while not (Atomic.get test_finished) do
    let out1 = Op.atomically c1 in
    let out2 = Op.atomically c2 in
    assert (not out1);
    assert (not out2)
  done

let test_casn () =
  let barrier = Barrier.make 3 in
  let test_finished = Atomic.make false in

  let a1 = Loc.make 0 in
  let a2 = Loc.make 0 in

  let domains = [ thread1; thread2; thread3 ] in
  List.map (fun f -> Domain.spawn (f barrier test_finished (a1, a2))) domains
  |> List.iter Domain.join

(* test 3 *)

let thread4 barrier test_finished (a1, a2) () =
  Barrier.await barrier;
  for i = 0 to nb_iter do
    let c = [ Op.make_cas a1 i (i + 1); Op.make_cas a2 i (i + 1) ] in
    assert (Op.atomically c)
  done;
  Atomic.set test_finished true

let thread5 barrier test_finished (a1, a2) () =
  Barrier.await barrier;
  while not (Atomic.get test_finished) do
    let a = Loc.get a1 in
    let b = Loc.get a2 in
    assert (a <= b)
  done

let test_read_casn () =
  let barrier = Barrier.make 2 in
  let test_finished = Atomic.make false in

  let a1 = Loc.make 0 in
  let a2 = Loc.make 0 in

  let domains = [ thread4; thread5 ] in
  List.map (fun f -> Domain.spawn (f barrier test_finished (a1, a2))) domains
  |> List.iter Domain.join

(* test 4 *)

let make_loc n =
  let rec loop n out =
    if n > 0 then loop (n - 1) (Loc.make 0 :: out) else out
  in
  loop n []

let make_kcas0 r_l =
  let rec loop r_l out =
    match r_l with h :: t -> loop t (Op.make_cas h 0 1 :: out) | [] -> out
  in
  loop r_l []

let make_kcas1 r_l =
  let rec loop r_l out =
    match r_l with h :: t -> loop t (Op.make_cas h 1 0 :: out) | [] -> out
  in
  loop r_l []

let test_stress n nb_loop =
  let r_l = make_loc n in
  let kcas0 = make_kcas0 r_l in
  let kcas1 = make_kcas1 r_l in
  for _ = 1 to nb_loop do
    assert (Op.atomically kcas0);
    assert (Op.atomically kcas1)
  done

(* test 5 *)

let test_presort () =
  let n_incs = 50_000 and n_domains = 3 and n_locs = 5 in

  let barrier = Barrier.make n_domains in

  let locs = Array.init n_locs (fun _ -> Loc.make 0) in

  let in_place_shuffle array =
    let n = Array.length array in
    for i = 0 to n - 2 do
      let j = Random.int (n - i) + i in
      let t = array.(i) in
      array.(i) <- array.(j);
      array.(j) <- t
    done
  in

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

  Array.make n_domains thread
  |> Array.map Domain.spawn |> Array.iter Domain.join;

  locs |> Array.iter (fun r -> assert (Loc.get r = n_incs * n_domains))

(* *)

let test_presort_and_is_in_log_xt () =
  let n_incs = 50_000 and n_domains = 3 and n_locs = 12 in
  let n_locs_half = n_locs asr 1 in

  let barrier = Barrier.make n_domains in

  let locs = Array.init n_locs (fun _ -> Loc.make 0) in

  let in_place_shuffle array =
    let n = Array.length array in
    for i = 0 to n - 2 do
      let j = Random.int (n - i) + i in
      let t = array.(i) in
      array.(i) <- array.(j);
      array.(j) <- t
    done
  in

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

  Array.make n_domains thread
  |> Array.map Domain.spawn |> Array.iter Domain.join;

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
       Xt.attempt ~mode:Mode.obstruction_free { tx }
     with Exit -> ());
    assert (!count = expect)
  in
  attempt_with_post_commit ~expect:0 { tx = (fun ~xt:_ -> raise Exit) };
  attempt_with_post_commit ~expect:1 { tx = (fun ~xt:_ -> ()) };
  let a = Loc.make 0 and b = Loc.make 0 in
  attempt_with_post_commit ~expect:1 { tx = Xt.modify a Fun.id };
  attempt_with_post_commit ~expect:1 { tx = Xt.incr a };
  attempt_with_post_commit ~expect:1
    { tx = (fun ~xt -> Xt.set ~xt a (Xt.exchange ~xt b 0)) }

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
  test_non_linearizable ();
  test_set ();
  test_casn ();
  test_read_casn ();
  test_stress 1000 10000;
  test_presort ();
  test_presort_and_is_in_log_xt ();
  test_updates ();
  test_post_commit ();
  test_backoff ();
  test_xt ()

(*
  ####
  *)
