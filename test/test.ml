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

open Kcas_lf;;
open Printf;;

let nb_iter = 100000;;
let wait_time = 15;;
let th1_success = ref true;;
let th2_success = ref true;;
let th3_success = ref true;;
let th4_success = ref true;;
let th5_success = ref true;;

let v_x = 0;;
let v_y = 1;;

let get r =
  match !r with
  |WORD(out) -> out
  |RDCSS_DESC(_) -> failwith "Error: expected WORD but a RDCSS_DESC was given !"
  |CASN_DESC(_) -> failwith "Error: expected WORD but a CASN_DESC was given !"
;;

let thread1 (a1, a2) =
  for i = 1 to nb_iter do
    let cd1 = [CAS (a1, WORD(v_x), WORD(v_y)) ; CAS (a2, WORD(v_x), WORD(v_y))] in
    let cd2 = [CAS (a1, WORD(v_y), WORD(v_x)) ; CAS (a2, WORD(v_y), WORD(v_x))] in
    let out1 = casn cd1 in
    let out2 = casn cd2 in
    if out1 <> true || out2 <> true then begin
      print_endline (sprintf "TH%d  APPEL N°%d ECHEC OUT1 = %b    OUT2 = %b!!!!!!!!!" (Domain.self ()) i out1 out2);
      th1_success := false
    end
  done;
  let cd1 = [CAS (a1, WORD(v_x), WORD(v_y)) ; CAS (a2, WORD(v_x), WORD(v_y))] in
  casn cd1;
  ()
;;

let thread2 (a1, a2) =
  for i = 1 to nb_iter do
    let cd1 = [CAS (a1, WORD(v_y), WORD(v_x)) ; CAS (a2, WORD(v_x), WORD(v_y))] in
    let cd2 = [CAS (a1, WORD(v_x), WORD(v_y)) ; CAS (a2, WORD(v_y), WORD(v_x))] in
    let out1 = casn cd1 in
    let out2 = casn cd2 in
    if out1 <> false || out2 <> false then begin
      print_endline (sprintf "TH%d  APPEL N°%d ECHEC OUT1 = %b    OUT2 = %b!!!!!!!!!" (Domain.self ()) i out1 out2);
      th2_success := false
    end
  done
;;

let thread3 (a1, a2) =
  for i = 1 to nb_iter do
    let cd1 = [CAS (a1, WORD(v_x), WORD(v_y)) ; CAS (a2, WORD(v_y), WORD(v_x))] in
    let cd2 = [CAS (a1, WORD(v_y), WORD(v_x)) ; CAS (a2, WORD(v_x), WORD(v_y))] in
    let out1 = casn cd1 in
    let out2 = casn cd2 in
    if out1 <> false || out2 <> false then begin
      print_endline (sprintf "TH%d  APPEL N°%d ECHEC OUT1 = %b    OUT2 = %b!!!!!!!!!" (Domain.self ()) i out1 out2);
      th3_success := false
    end
  done
;;

let thread4 (a1, a2) =
  for i = 0 to nb_iter do
    let cd = [CAS (a1, WORD(i), WORD(i+1)) ; CAS (a2, WORD(i), WORD(i+1))] in
    let out = casn cd in
    if out <> true then
      th4_success := false
  done
;;

let thread5 (a1, a2) =
  for i = 0 to nb_iter do
    let a = casn_read a1 in
    let b = casn_read a2 in
    if a > b then
      th5_success := false
  done
;;

let test_casn () =
  let a1 = mk_ref v_x in
  let a2 = mk_ref v_x in

  Domain.spawn (fun () -> thread1 (a1, a2));
  Domain.spawn (fun () -> thread2 (a1, a2));
  Domain.spawn (fun () -> thread3 (a1, a2));

  Unix.sleep wait_time;
  print_endline (sprintf "a1 = %d et a2 = %d" (get a1) (get a2));
  !th1_success && !th2_success && !th3_success
;;

let test_read_casn () =
  let a1 = mk_ref 0 in
  let a2 = mk_ref 0 in

  Domain.spawn (fun () -> thread4 (a1, a2));
  Domain.spawn (fun () -> thread5 (a1, a2));

  Unix.sleep wait_time;
  !th4_success && !th5_success
;;

let () =
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





















(*
####
*)
