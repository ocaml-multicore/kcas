(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

type state =
  |WORD of int
  |RDCSS_DESC of ref * state * ref * state * state
  |CASN_DESC of ref * t list
and t = CAS : ref * state * state -> t
and ref = {
  mutable content : state;
  id : int;
};;

let compare_and_swap r x y =
  Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y)
;;
(*
let ref x = {
  content = WORD(x);
  id = Oo.id (object end);
};;*)
let ref x = {
  content = WORD(x);
  id = Random.bits ();
};;
(*
let mk_rdcss_desc c1 c2 =
  DESC(RDCSS_DESC, ref 0, [c1 ; c2])
;;
*)

let rec ref_eq r r' =
(*   print_endline (sprintf "id1 = %d ?= %d = id2" r.id r'.id); *)
  r.id = r'.id
and st_eq s s' =
  match s, s' with
  |WORD(x), WORD(x') ->
(*     print_endline (sprintf "x = %d ?= %d = x'" x x'); *)
    x = x'
  |RDCSS_DESC(a1, o1, a2, o2, n2), RDCSS_DESC(a1', o1', a2', o2', n2') ->
    a1==a1' && o1==o1' && a2==a2' && o2==o2' && n2==n2'
(*     print_endline "COMPARAISON RDCSS"; *)
(*   ref_eq a1 a1' && st_eq o1 o1' && ref_eq a2 a2' && st_eq o2 o2' && st_eq n2 n2' *)
  |CASN_DESC(s, c), CASN_DESC(s', c') ->
    s == s' && c == c'
(*     print_endline "COMPARAISON CASN"; *)
(*     s ==  s' && *)
(*     List.for_all2 (fun (CAS(a, o, n)) (CAS(a', o', n')) -> ref_eq a a' && st_eq o o' && st_eq n n') c c' *)
  |_ -> false
;;


let cas1' a o n =
  let s = a.content in
  match s, o with
  |WORD(x), WORD(x') when x == x' -> compare_and_swap a s n
  |RDCSS_DESC(a1, o1, a2, o2, n2), RDCSS_DESC(a1', o1', a2', o2', n2') when a1==a1' && o1==o1' && a2==a2' && o2==o2' && n2==n2' ->
    compare_and_swap a s n
  |CASN_DESC(s, l), CASN_DESC(s', l') when s == s' && l == l' ->
    compare_and_swap a s n
  |_ -> false
;;

let cas1 a o n =
  let s = a.content in
  st_eq s o && compare_and_swap a s n
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

let rec rdcss a1 o1 a2 o2 n2 =
  if commit (CAS(a2, o2, RDCSS_DESC(a1, o1, a2, o2, n2))) then begin
(*     print_endline (sprintf "TH%d: RDCSS COMMIT SUCCESS" (Domain.self ())); *)
    complete a1 o1 a2 o2 n2; o2
  end else
    let r = a2.content in
    match r with
    |RDCSS_DESC(a1', o1', a2', o2', n2') -> complete a1' o1' a2' o2' n2'; rdcss a1 o1 a2 o2 n2
    |_ -> if st_eq r o2 then
            rdcss a1 o1 a2 o2 n2
          else
            r
and complete a1 o1 a2 o2 n2 =
(*   print_endline "COMPLETE"; *)
  if st_eq a1.content o1 then begin
(*     print_endline (sprintf "TH%d: COMPLETE SUCCESS" (Domain.self ())); *)
    let out = commit (CAS(a2, RDCSS_DESC(a1, o1, a2, o2, n2), n2)) in
(*     print_endline (sprintf "TH%d: COMPLETE SUCCESS %b" (Domain.self ()) out); *)
    out
  end else begin
(*     print_endline (sprintf "TH%d: COMPLETE FAILED" (Domain.self ())); *)
    let out = commit (CAS(a2, RDCSS_DESC(a1, o1, a2, o2, n2), o2)) in
(*     print_endline (sprintf "TH%d: COMPLETE FAILED %b" (Domain.self ()) out); *)
    out
  end
;;

let rec rdcss_read a =
  let r = a.content in
  match r with
  |RDCSS_DESC(a1, o1, a2, o2, n2) -> complete a1 o1 a2 o2 n2; rdcss_read a
  |_ -> r
;;

(* UNDECIDED : 0    FAILED : 1    SUCCEEDED : 2 *)
let rec casn_proceed st c_l =
(*   print_endline (sprintf "TH%d: ID %d  " (Domain.self ()) st.id); *)
  let rec phase1 curr_c_l curr_st out =
    match curr_c_l with
    |(CAS(a, o, n))::curr_c_t_tail when curr_st = 2 -> begin
      let cd = CASN_DESC(st, c_l) in
      let s = rdcss st (WORD(0)) a o cd in
      match s with
      |CASN_DESC(st', c_l') as cd' ->
        if not (st_eq cd s) then begin
(*           print_endline (sprintf "TH%d: Other CASN DESC !!" (Domain.self ())); *)
          casn_proceed st' c_l'; phase1 curr_c_l curr_st out
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
      commit (CAS(st, (WORD(0)), (WORD(curr_st)))); out
  in
  let rec phase2 curr_c_l succ =
    match curr_c_l with
    |(CAS(a, o, n))::curr_c_l_tail -> begin
      (match succ with
       |WORD(2) ->
(*          print_endline "ICI"; *)
         let out = commit (CAS(a, CASN_DESC(st, c_l), n)) in
(*          print_endline (sprintf "TH%d: len = %d    out = %b" (Domain.self ()) (List.length curr_c_l) out); *)
         out
       |_ -> commit (CAS(a, CASN_DESC(st, c_l), o)));
      phase2 curr_c_l_tail succ
    end
    |_ -> succ = WORD(2)
  in
  let cas_to_proceed =
    if st.content = WORD(0) then
      phase1 c_l 2 []
    else
      c_l
  in
  phase2 cas_to_proceed st.content
;;

let casn c_l = casn_proceed (ref 0) c_l;;

let rec casn_read a =
  let r = rdcss_read a in
  match r with
  |CASN_DESC(st, c_l) -> casn_proceed st c_l; casn_read a
  |WORD(out) -> out
  |_ -> assert false
;;













(*
####
*)
