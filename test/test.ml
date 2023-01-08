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

let nb_iter = 10_000

let assert_kcas ref expected_v = 
  let present_v = Kcas.get ref in 
  assert (present_v == expected_v)
;;

let test_set () =
  let a = Kcas.ref 0 in
  assert_kcas a 0;
  Kcas.set a 1;
  assert_kcas a 1

let thread1 (a1, a2) =
  let c1 = [ Kcas.mk_cas a1 0 1; Kcas.mk_cas a2 0 1 ] in
  let c2 = [ Kcas.mk_cas a1 1 0; Kcas.mk_cas a2 1 0 ] in
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
  ignore @@ Kcas.kCAS c1

let thread2 (a1, a2) =
  let c1 = [ Kcas.mk_cas a1 1 0; Kcas.mk_cas a2 0 1 ] in
  let c2 = [ Kcas.mk_cas a1 0 1; Kcas.mk_cas a2 1 0 ] in
  for _ = 1 to nb_iter do
    let out1 = Kcas.kCAS c1 in
    let out2 = Kcas.kCAS c2 in
    assert (not out1);
    assert (not out2)
  done

let thread3 (a1, a2) =
  let c1 = [ Kcas.mk_cas a1 0 1; Kcas.mk_cas a2 1 0 ] in
  let c2 = [ Kcas.mk_cas a1 1 0; Kcas.mk_cas a2 0 1 ] in
  for _ = 1 to nb_iter do
    let out1 = Kcas.kCAS c1 in
    let out2 = Kcas.kCAS c2 in
    assert (not out1);
    assert (not out2)
  done

let test_casn () =
  let a1 = Kcas.ref 0 in
  let a2 = Kcas.ref 0 in

  let domains =
    [
      (fun () -> thread1 (a1, a2));
      (fun () -> thread2 (a1, a2));
      (fun () -> thread3 (a1, a2));
    ]
  in
  List.map Domain.spawn domains |> List.iter Domain.join
  
let thread4 (a1, a2) =
  for i = 0 to nb_iter do
    let c = [ Kcas.mk_cas a1 i (i + 1); Kcas.mk_cas a2 i (i + 1) ] in
    assert (Kcas.kCAS c)
  done

let thread5 (a1, a2) =
  for _ = 0 to nb_iter do
    let a = Kcas.get a1 in
    let b = Kcas.get a2 in
    assert (a <= b)
  done

let test_read_casn () =
  let a1 = Kcas.ref 0 in
  let a2 = Kcas.ref 0 in

  let domains =
    [ (fun () -> thread4 (a1, a2)); (fun () -> thread5 (a1, a2)) ]
  in
  List.map Domain.spawn domains |> List.iter Domain.join

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

let () =
  test_set ();
  test_casn ();
  test_read_casn ();
  test_set ();
  test_stress 1000 10000

(*
####
*)
