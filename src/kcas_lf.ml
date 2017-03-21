(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

type state =
  |WORD of int
  |RDCSS_DESC of rdcss_t
  |CASN_DESC of casn_t
and t = CAS : ref * state * state -> t
and ref = {
  mutable content : state;
  id : int;
}
and rdcss_t = {
  a1 : ref;
  o1 : state;
  a2 : ref;
  o2 : state;
  n2 : state;
  id : int;
}
and casn_t = {
  st : ref;
  c_l : t list;
  id : int;
};;
(*
type 'a state =
  |WORD : 'a -> 'a state
  |RDCSS_DESC : status ref * status state * 'a ref * 'a state * 'a state -> 'a state
  |CASN_DESC : status ref * t list -> 'a state
and t = CAS : 'a ref * 'a state * 'a state -> t
and status = |UNDECIDED |FAILED |SUCCEEDED
and 'a ref = {
  mutable content : 'a state;
  id : int;
};;*)

let compare_and_swap r x y =
  Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y)
;;

let ref x = {
  content = WORD(x);
  id = Oo.id (object end);
};;

let mk_rdcss a1 o1 a2 o2 n2 = {
  a1 = a1;
  o1 = o1;
  a2 = a2;
  o2 = o2;
  n2 = n2;
  id = Oo.id (object end);
};;

let mk_casn st c_l = {
  st = st;
  c_l = c_l;
  id = Oo.id (object end);
};;

let rec st_eq s s' =
  match s, s' with
  |WORD(x), WORD(x') ->
(*     print_endline (sprintf "x = %d ?= %d = x'" x x'); *)
    x == x'
  |RDCSS_DESC(r), RDCSS_DESC(r') ->
(*     ref_eq a1 a1' && o1==o1' && ref_eq a2 a2' && o2==o2' && n2==n2' *)
(*     print_endline "COMPARAISON RDCSS"; *)
  r.id == r'.id
(*   ref_eq a1 a1' && st_eq o1 o1' && ref_eq a2 a2' && st_eq o2 o2' && st_eq n2 n2' *)
  |CASN_DESC(c), CASN_DESC(c') ->
(*     s == s' && c == c' *)
(*     print_endline "COMPARAISON CASN"; *)
(*     ref_eq s s' && *)
(*     List.for_all2 (fun (CAS(a, o, n)) (CAS(a', o', n')) -> ref_eq a a' && st_eq o o' && st_eq n n') c c' *)
    c.id == c'.id
  |_ -> false
;;

(*
let cas1' a o n =
  let s = a.content in
  match s, o with
  |WORD(x), WORD(x') when x == x' -> compare_and_swap a s n
  |RDCSS_DESC(a1, o1, a2, o2, n2), RDCSS_DESC(a1', o1', a2', o2', n2') when a1==a1' && o1==o1' && a2==a2' && o2==o2' && n2==n2' ->
    compare_and_swap a s n
  |CASN_DESC(s, l), CASN_DESC(s', l') when s == s' && l == l' ->
    compare_and_swap a s n
  |_ -> false
;;*)

let cas1 a o n =
  let s = a.content in
  if st_eq s o then
    compare_and_swap a s n
  else
    false
;;


let commit (CAS (r, expect, update)) =
  cas1 r expect update
;;

let is_descriptor d =
  match d with
  |RDCSS_DESC(_) -> true
  |_ -> false
;;

let is_casn d =
  match d with
  |CASN_DESC(_) -> true
  |_ -> false
;;

let rec rdcss rd =
  if commit (CAS(rd.a2, rd.o2, RDCSS_DESC(rd))) then begin
(*     print_endline (sprintf "TH%d: RDCSS COMMIT SUCCESS" (Domain.self ())); *)
    complete rd; rd.o2
  end else
    let r = rd.a2.content in
    match r with
    |RDCSS_DESC(rd') -> complete rd'; rdcss rd
    |_ -> if st_eq r rd.o2 then
            rdcss rd
          else
            r
and complete rd =
(*   print_endline "COMPLETE"; *)
  if st_eq rd.a1.content rd.o1 then begin
(*     print_endline (sprintf "TH%d: COMPLETE SUCCESS" (Domain.self ())); *)
    let out = commit (CAS(rd.a2, RDCSS_DESC(rd), rd.n2)) in
(*     print_endline (sprintf "TH%d: COMPLETE SUCCESS %b" (Domain.self ()) out); *)
    out
  end else begin
(*     print_endline (sprintf "TH%d: COMPLETE FAILED" (Domain.self ())); *)
    let out = commit (CAS(rd.a2, RDCSS_DESC(rd), rd.o2)) in
(*     print_endline (sprintf "TH%d: COMPLETE FAILED %b" (Domain.self ()) out); *)
    out
  end
;;

let rec rdcss_read a =
  let r = a.content in
  match r with
  |RDCSS_DESC(rd) -> complete rd; rdcss_read a
  |_ -> r
;;

(* UNDECIDED : 0    FAILED : 1    SUCCEEDED : 2 *)
let rec casn_proceed c =
(*   print_endline (sprintf "TH%d: ID %d  " (Domain.self ()) st.id); *)
  let rec phase1 curr_c_l curr_st out =
    match curr_c_l with
    |(CAS(a, o, n))::curr_c_t_tail when curr_st = 2 -> begin
      let s = rdcss (mk_rdcss c.st (WORD(0)) a o (CASN_DESC(c))) in
      match s with
      |CASN_DESC(c') ->
        if c.id != c'.id then begin
(*           print_endline (sprintf "TH%d: Other CASN DESC !!" (Domain.self ())); *)
          casn_proceed c'; phase1 curr_c_l curr_st out
        end else begin
(*           print_endline (sprintf "TH%d: Same CASN DESC !!" (Domain.self ())); *)
          phase1 curr_c_t_tail curr_st (CAS(a, o, n)::out)
        end
      |_ ->
        if st_eq s o then begin
(*           print_endline (sprintf "TH%d: CASN STEP SUCCESS" (Domain.self ())); *)
          phase1 curr_c_t_tail curr_st (CAS(a, o, n)::out)
        end else begin
(*           print_endline (sprintf "TH%d: CASN STEP FAILED" (Domain.self ())); *)
          phase1 curr_c_t_tail 1 (CAS(a, o, n)::out)
        end
    end
    |_ ->
(*       print_endline (sprintf "TH%d: CASN PHASE1 FIN" (Domain.self ())); *)
      commit (CAS(c.st, (WORD(0)), (WORD(curr_st)))); out
  in
  let rec phase2 curr_c_l succ =
    match curr_c_l with
    |(CAS(a, o, n))::curr_c_l_tail -> begin
      (match succ.content with
       |WORD(2) ->
(*          print_endline "ICI"; *)
         let out = commit (CAS(a, CASN_DESC(c), n)) in
(*          print_endline (sprintf "TH%d: len = %d    out = %b" (Domain.self ()) (List.length curr_c_l) out); *)
         out
       |_ -> commit (CAS(a, CASN_DESC(c), o)));
      phase2 curr_c_l_tail succ
    end
    |_ -> succ.content = WORD(2)
  in
  match c.st.content with
  |WORD(0) -> phase2 (phase1 c.c_l 2 []) c.st
  |_ -> phase2 c.c_l c.st
;;

let casn c_l = casn_proceed (mk_casn (ref 0) c_l);;

let rec casn_read a =
  let r = rdcss_read a in
  match r with
  |CASN_DESC(c) -> casn_proceed c; casn_read a
  |WORD(out) -> out
  |_ -> assert false
;;













(*
####
*)
