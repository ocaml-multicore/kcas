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

open Kcas;;
open Printf;;

let nb_iter = 10000;;
let wait_time = 2;;
let th1_success = Stdlib.ref true;;
let th2_success = Stdlib.ref true;;
let th3_success = Stdlib.ref true;;
let th4_success = Stdlib.ref true;;
let th5_success = Stdlib.ref true;;

let v_x = 0;;
let v_y = 1;;

let thread1 (a1, a2) =
  let c1 = [mk_cas a1 v_x v_y ; mk_cas a2 v_x v_y] in
  let c2 = [mk_cas a1 v_y v_x ; mk_cas a2 v_y v_x] in
  for _ = 1 to nb_iter do
    let out1 = kCAS c1 in
    let out2 = kCAS c2 in
    if out1 <> true || out2 <> true then begin
(*      print_endline (sprintf "TH%d  APPEL N°%d ECHEC OUT1 = %b    OUT2 = %b!!!!!!!!!" (Domain.self ()) i out1 out2);*)
      th1_success := false
    end
  done;
  ignore @@ kCAS c1;
  ()
;;

let thread2 (a1, a2) =
  let c1 = [mk_cas a1 v_y v_x ; mk_cas a2 v_x v_y] in
  let c2 = [mk_cas a1 v_x v_y ; mk_cas a2 v_y v_x] in
  for _ = 1 to nb_iter do
    let out1 = kCAS c1 in
    let out2 = kCAS c2 in
    if out1 <> false || out2 <> false then begin
(*      print_endline (sprintf "TH%d  APPEL N°%d ECHEC OUT1 = %b    OUT2 = %b!!!!!!!!!" (Domain.self ()) i out1 out2);*)
      th2_success := false
    end
  done
;;

let thread3 (a1, a2) =
  let c1 = [mk_cas a1 v_x v_y ; mk_cas a2 v_y v_x] in
  let c2 = [mk_cas a1 v_y v_x ; mk_cas a2 v_x v_y] in
  for _ = 1 to nb_iter do
    let out1 = kCAS c1 in
    let out2 = kCAS c2 in
    if out1 <> false || out2 <> false then begin
(*      print_endline (sprintf "TH%d  APPEL N°%d ECHEC OUT1 = %b    OUT2 = %b!!!!!!!!!" (Domain.self ()) i out1 out2);*)
      th3_success := false
    end
  done
;;

let thread4 (a1, a2) =
  for i = 0 to nb_iter do
    let c = [mk_cas a1 i (i+1) ; mk_cas a2 i (i+1)] in
    let out = kCAS c in
    if out <> true then
      th4_success := false
  done
;;

let thread5 (a1, a2) =
  for _ = 0 to nb_iter do
    let a = get a1 in
    let b = get a2 in
    if a > b then
      th5_success := false
  done
;;

let test_casn () =
  let a1 = ref v_x in
  let a2 = ref v_x in

  Domain.spawn (fun () -> thread1 (a1, a2)) |> ignore;
  Domain.spawn (fun () -> thread2 (a1, a2)) |> ignore;
  Domain.spawn (fun () -> thread3 (a1, a2)) |> ignore;

  Unix.sleep wait_time;
  print_endline (sprintf "a1 = %d et a2 = %d" (get a1) (get a2));
  !th1_success && !th2_success && !th3_success
;;

let test_read_casn () =
  let a1 = ref 0 in
  let a2 = ref 0 in

  Domain.spawn (fun () -> thread4 (a1, a2)) |> ignore;
  Domain.spawn (fun () -> thread5 (a1, a2)) |> ignore;

  Unix.sleep wait_time;
  !th4_success && !th5_success
;;

let test_set () =
  let a = ref 0 in
  let v1 = get a in
  set a 1;
  let v2 = get a in
  print_endline (sprintf "V1 = %d et V2 = %d" v1 v2)
;;

let main_test () =
  print_endline (sprintf "Test CASN");
  if test_casn () then
    print_endline (sprintf "SUCCEEDED")
  else
    print_endline (sprintf "FAILED");
  print_endline (sprintf "Test READ CASN");
  if test_read_casn () then
    print_endline (sprintf "SUCCEEDED")
  else
    print_endline (sprintf "FAILED");
  print_endline (sprintf "\nEND")
;;

let make_ref n =
  let rec loop n out =
    if n > 0 then
      loop (n-1) ((ref 0)::out)
    else
      out
  in loop n []
;;

let make_kcas0 r_l =
  let rec loop r_l out =
    match r_l with
    |h::t -> loop t ((mk_cas h 0 1)::out)
    |[] -> out
  in loop r_l []
;;

let make_kcas1 r_l =
  let rec loop r_l out =
    match r_l with
    |h::t -> loop t ((mk_cas h 1 0)::out)
    |[] -> out
  in loop r_l []
;;

let test_benchmark n nb_loop =
  let r_l = make_ref n in
  let kcas0 = make_kcas0 r_l in
  let kcas1 = make_kcas1 r_l in
  let rec loop nb_loop =
    if nb_loop > 0 then
      if kCAS kcas0 then
        if kCAS kcas1 then
          loop (nb_loop-1)
        else
          print_endline (sprintf "FAILURE KCAS1 n°%d" nb_loop)
      else
        print_endline (sprintf "FAILURE KCAS0 n°%d" nb_loop)
  in loop nb_loop;
  print_endline "TEST SUCCEED"
;;

let () =
  main_test ()
(*  test_set ();*)
(*  test_benchmark 1000 10000*)
;;





















(*
####
*)
