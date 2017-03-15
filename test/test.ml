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
let th1_success = Pervasives.ref true;;
let th2_success = Pervasives.ref true;;
let th3_success = Pervasives.ref true;;
let th4_success = Pervasives.ref true;;
let th5_success = Pervasives.ref true;;

let thread1 (a1, a2) =
  for i = 1 to nb_iter do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(0), WORD(1))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(1), WORD(0))]) in
    let out1 = casn cd1 in
    let out2 = casn cd2 in
    if out1 <> true || out2 <> true then
      th1_success := false
  done
;;

let thread2 (a1, a2) =
  for i = 1 to nb_iter do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
    let out1 = casn cd1 in
    let out2 = casn cd2 in
    if out1 <> false || out2 <> false then
      th2_success := false
  done
;;

let thread3 (a1, a2) =
  for i = 1 to nb_iter do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
    let out1 = casn cd1 in
    let out2 = casn cd2 in
    if out1 <> false || out2 <> false then
      th3_success := false
  done
;;

let thread4 (a1, a2) =
  for i = 0 to nb_iter do
    let cd = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(i), WORD(i+1)) ; CAS (a2, WORD(i), WORD(i+1))]) in
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
  let a1 = ref 0 in
  let a2 = ref 0 in

  Domain.spawn (fun () -> thread1 (a1, a2));
  Domain.spawn (fun () -> thread2 (a1, a2));
  Domain.spawn (fun () -> thread3 (a1, a2));

  Unix.sleep 15;
  !th1_success && !th2_success && !th3_success
;;

let test_read_casn () =
  let a1 = ref 0 in
  let a2 = ref 0 in

  Domain.spawn (fun () -> thread4 (a1, a2));
  Domain.spawn (fun () -> thread5 (a1, a2));

  Unix.sleep 15;
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
