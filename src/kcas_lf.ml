(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Printf;;

type state =
  |WORD of int
  |DESC of desc_t * ref * t list
and t = CAS : ref * state * state -> t
and desc_t =
  |RDCSS_DESC
  |CASN_DESC
and ref = {
  mutable content : state;
  id : int;
};;

let compare_and_swap r x y =
  Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y)
;;

let ref x = {
  content = WORD(x);
  id = Oo.id (object end);
};;

let mk_rdcss_desc c1 c2 =
  DESC(RDCSS_DESC, ref 0, [c1 ; c2])
;;

let cas1 a o n =
  let s = a.content in
  match s, o with
  |WORD(x), WORD(e) when x == e ->
    compare_and_swap a s n
  |DESC(d1, s1, l1), DESC(d2, s2, l2) when d1 == d2 && s1 == s2 && l1 == l2 ->
    compare_and_swap a s n
  |_ -> false
;;

let commit (CAS (r, expect, update)) =
  cas1 r expect update
;;

let is_descriptor d =
  match d with
  |DESC(d_t, _, _) -> d_t = RDCSS_DESC
  |WORD(_) -> false
;;

let is_casn d =
  match d with
  |DESC(d_t, _, _) -> d_t = CASN_DESC
  |WORD(_) -> false
;;

let rec rdcss d =
  match d with
  |DESC(RDCSS_DESC, st, (CAS (a1, o1, n1))::(CAS (a2, o2, n2))::_) ->
    if commit (CAS(a2, o2, d)) then
       (complete d; o2)
    else
      let r = a2.content in
      if is_descriptor r then (complete r; rdcss d)
      else if r = o2 then rdcss d
      else r
  | _ -> failwith "Rdcss Invalid Argument"
and complete d =
  match d with
  |DESC(RDCSS_DESC, st, (CAS (a1, o1, n1))::(CAS (a2, o2, n2))::_) ->
    if a1.content = o1 then
      commit (CAS(a2, d, n2))
    else
      commit (CAS(a2, d, o2))
  | _ -> failwith "Complete Invalid Argument"
;;

let rec rdcss_read a =
  let r = a.content in
  if is_descriptor r then begin
    complete r; rdcss_read a
  end else
    r
;;

(* UNDECIDED : 0    FAILED : 1    SUCCEEDED : 2 *)
let rec casn cd =
  match cd with
  |DESC(CASN_DESC, st, c_l) ->
    let rec phase1 c_l curr_st out =
      match c_l with
      |(CAS(a, o, n))::c_t_tail when curr_st = 2 ->
        let s = rdcss (mk_rdcss_desc (CAS(st, WORD(0), WORD(0))) (CAS(a, o, cd))) in
        if is_casn s && s != cd then begin
          casn s; phase1 c_l curr_st out
        end else
          let new_curr_st = if is_casn s || s = o then curr_st else 1 in
          phase1 c_t_tail new_curr_st ((CAS(a, o, n))::out)
      |_ -> commit (CAS(st, (WORD(0)), (WORD(curr_st)))); out
    in
    let rec phase2 c_l succ =
      match c_l with
      |(CAS(a, o, n))::c_t_tail -> begin
        (match succ with
         |WORD(2) -> commit (CAS(a, cd, n))
         |_ -> commit (CAS(a, cd, o)));
        phase2 c_t_tail succ
      end
      |[] -> succ = WORD(2)
    in
    let cas_to_proceed =
      if st.content = WORD(0) then
        phase1 c_l 2 []
      else
        c_l
    in
    phase2 cas_to_proceed st.content
  |_ -> false
;;

let rec casn_read a =
  let r = rdcss_read a in
  if is_casn r then begin
    casn r; casn_read a
  end else
    match r with
    |WORD(out) -> out
    |_ -> assert false
;;













(*
####
*)
