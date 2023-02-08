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

let nb_iter = 100_000

let assert_kcas ref expected_v =
  let present_v = Kcas.get ref in
  assert (present_v == expected_v)

module Barrier = struct
  type t = { counter : int Atomic.t; total : int }

  let make total = { counter = Atomic.make 0; total }

  let await { counter; total } =
    Atomic.incr counter;
    while Atomic.get counter < total do
      ()
    done
end

let test_non_linearizable () =
  let barrier = Barrier.make 2
  and n_iter = 1_000_000
  and test_finished = ref false in

  let a = Kcas.ref 0 and b = Kcas.ref 0 in

  let cass1a = [ Kcas.mk_cas b 0 0; Kcas.mk_cas a 0 1 ]
  and cass1b = [ Kcas.mk_cas b 0 0; Kcas.mk_cas a 1 0 ]
  and cass2a = [ Kcas.mk_cas b 0 1; Kcas.mk_cas a 0 0 ]
  and cass2b = [ Kcas.mk_cas b 1 0; Kcas.mk_cas a 0 0 ] in

  let thread1 () =
    Barrier.await barrier;
    while not !test_finished do
      if Kcas.kCAS cass1a then
        while not (Kcas.kCAS cass1b) do
          assert (Kcas.get a == 1 && Kcas.get b == 0)
        done
    done
  and thread2 () =
    Barrier.await barrier;
    for _ = 1 to n_iter do
      if Kcas.kCAS cass2a then
        while not (Kcas.kCAS cass2b) do
          assert (Kcas.get a == 0 && Kcas.get b == 1)
        done
    done;
    test_finished := true
  in

  [ thread2; thread1 ] |> List.map Domain.spawn |> List.iter Domain.join

(* test 1 *)
let test_set () =
  let a = Kcas.ref 0 in
  assert_kcas a 0;
  Kcas.set a 1;
  assert_kcas a 1

(* test 2 *)
let thread1 barrier test_finished (a1, a2) () =
  let c1 = [ Kcas.mk_cas a1 0 1; Kcas.mk_cas a2 0 1 ] in
  let c2 = [ Kcas.mk_cas a1 1 0; Kcas.mk_cas a2 1 0 ] in

  Barrier.await barrier;

  for _ = 1 to nb_iter do
    assert_kcas a1 0;
    assert_kcas a2 0;

    let out1 = Kcas.kCAS c1 in
    assert out1;

    assert_kcas a1 1;
    assert_kcas a2 1;

    let out2 = Kcas.kCAS c2 in
    assert out2
  done;
  Atomic.set test_finished true

let thread2 barrier test_finished (a1, a2) () =
  let c1 = [ Kcas.mk_cas a1 1 0; Kcas.mk_cas a2 0 1 ] in
  let c2 = [ Kcas.mk_cas a1 0 1; Kcas.mk_cas a2 1 0 ] in

  Barrier.await barrier;

  while not (Atomic.get test_finished) do
    let out1 = Kcas.kCAS c1 in
    let out2 = Kcas.kCAS c2 in
    assert (not out1);
    assert (not out2)
  done

let thread3 barrier test_finished (a1, a2) () =
  let c1 = [ Kcas.mk_cas a1 0 1; Kcas.mk_cas a2 1 0 ] in
  let c2 = [ Kcas.mk_cas a1 1 0; Kcas.mk_cas a2 0 1 ] in

  Barrier.await barrier;

  while not (Atomic.get test_finished) do
    let out1 = Kcas.kCAS c1 in
    let out2 = Kcas.kCAS c2 in
    assert (not out1);
    assert (not out2)
  done

let test_casn () =
  let barrier = Barrier.make 3 in
  let test_finished = Atomic.make false in

  let a1 = Kcas.ref 0 in
  let a2 = Kcas.ref 0 in

  let domains = [ thread1; thread2; thread3 ] in
  List.map (fun f -> Domain.spawn (f barrier test_finished (a1, a2))) domains
  |> List.iter Domain.join

(* test 3 *)

let thread4 barrier test_finished (a1, a2) () =
  Barrier.await barrier;
  for i = 0 to nb_iter do
    let c = [ Kcas.mk_cas a1 i (i + 1); Kcas.mk_cas a2 i (i + 1) ] in
    assert (Kcas.kCAS c)
  done;
  Atomic.set test_finished true

let thread5 barrier test_finished (a1, a2) () =
  Barrier.await barrier;
  while not (Atomic.get test_finished) do
    let a = Kcas.get a1 in
    let b = Kcas.get a2 in
    assert (a <= b)
  done

let test_read_casn () =
  let barrier = Barrier.make 2 in
  let test_finished = Atomic.make false in

  let a1 = Kcas.ref 0 in
  let a2 = Kcas.ref 0 in

  let domains = [ thread4; thread5 ] in
  List.map (fun f -> Domain.spawn (f barrier test_finished (a1, a2))) domains
  |> List.iter Domain.join

(* test 4 *)

let make_ref n =
  let rec loop n out =
    if n > 0 then loop (n - 1) (Kcas.ref 0 :: out) else out
  in
  loop n []

let make_kcas0 r_l =
  let rec loop r_l out =
    match r_l with h :: t -> loop t (Kcas.mk_cas h 0 1 :: out) | [] -> out
  in
  loop r_l []

let make_kcas1 r_l =
  let rec loop r_l out =
    match r_l with h :: t -> loop t (Kcas.mk_cas h 1 0 :: out) | [] -> out
  in
  loop r_l []

let test_stress n nb_loop =
  let r_l = make_ref n in
  let kcas0 = make_kcas0 r_l in
  let kcas1 = make_kcas1 r_l in
  for _ = 1 to nb_loop do
    assert (Kcas.kCAS kcas0);
    assert (Kcas.kCAS kcas1)
  done

(* test 5 *)

let test_presort () =
  let n_incs = 50_000 and n_domains = 3 and n_refs = 5 in

  let barrier = Barrier.make n_domains in

  let refs = Array.init n_refs (fun _ -> Kcas.ref 0) in

  let in_place_shuffle array =
    let n = Array.length array in
    for i = 0 to n - 2 do
      let j = Random.int (n - i) + i in
      let t = array.(i) in
      array.(i) <- array.(j);
      array.(j) <- t
    done
  in

  let mk_inc refs =
    in_place_shuffle refs;
    let x = Kcas.get refs.(0) in
    let y = x + 1 in
    Array.fold_left (fun cs r -> Kcas.mk_cas r x y :: cs) [] refs
  in

  let thread () =
    let refs = Array.copy refs in
    Random.self_init ();
    Barrier.await barrier;
    for _ = 1 to n_incs do
      while not (Kcas.kCAS (mk_inc refs)) do
        ()
      done
    done
  in

  Array.make n_domains thread
  |> Array.map Domain.spawn |> Array.iter Domain.join;

  refs |> Array.iter (fun r -> assert (Kcas.get r = n_incs * n_domains))

let () =
  test_non_linearizable ();
  test_set ();
  test_casn ();
  test_read_casn ();
  test_stress 1000 10000;
  test_presort ()

(*
  ####
  *)
