(*

Compilation :
  ocamlc -thread unix.cma threads.cma kcas_lf.ml
  ocamlopt -thread unix.cmxa threads.cmxa kcas_lf.ml


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

let get_id {id; _} = id;;

let compare_and_swap : ref -> state -> state -> state = fun r x y ->
  Obj.obj (Obj.compare_and_swap_field_val (Obj.repr r) 0 (Obj.repr x) (Obj.repr y))
;;

let compare_and_swap_bool r x y =
  Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y)
;;

let ref x = {
  content = WORD(x);
  id = Oo.id (object end);
};;

let word w =
  match w with
  |WORD(a) -> sprintf "%d" a
  |DESC(d, _, _) ->
    if d = RDCSS_DESC then
      "RDCSS_DESC"
    else
      "CASN_DESC"
;;

let get r = word r.content;;

let mk_rdcss_desc c1 c2 =
  DESC(RDCSS_DESC, ref 0, [c1 ; c2])
;;

let cas1 a o n =
  let s = a.content in
  match s, o with
  |WORD(x), WORD(e) when x == e ->
    print_endline (sprintf "TH%d: CAS WORD" (Domain.self ()));
    let out = (compare_and_swap a s n) in
    (* print_endline (sprintf "TH%d: CAS WORD >>>> %s" (Domain.self ()) (word out)); *)
    out
  |DESC(d1, s1, l1), DESC(d2, s2, l2) when d1 == d2 && s1 == s2 && l1 == l2 ->
    print_endline (sprintf "TH%d: CAS DESC" (Domain.self ()));
    (compare_and_swap a s n)
  |_ ->
    print_endline (sprintf "TH%d: CAS NONE" (Domain.self ()));
    s
;;

let cas1_bis a o n =
  let s = a.content in
  match s, o with
  |WORD(x), WORD(e) when x == e ->
    (* print_endline (sprintf "TH%d: CAS WORD" (Domain.self ())); *)
    compare_and_swap_bool a s n
  |DESC(d1, s1, l1), DESC(d2, s2, l2) when d1 == d2 && s1 == s2 && l1 == l2 ->
    (* print_endline (sprintf "TH%d: CAS DESC" (Domain.self ())); *)
    compare_and_swap_bool a s n
  |_ ->
    (* print_endline (sprintf "TH%d: CAS NONE" (Domain.self ())); *)
    false
;;

let commit (CAS (r, expect, update)) =
  let out = cas1 r expect update in
  (* print_endline (sprintf "TH%d: COMMIT >>>> %s" (Domain.self ()) (word out)); *)
  out
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

let rdcss_str d =
  match d with
  |DESC(RDCSS_DESC, st, (CAS (a1, o1, n1))::(CAS (a2, o2, n2))::_) ->
    sprintf "(%s, %s, %s, %s, %s)" (get a1) (word o1) (get a2) (word o2) (word n2)
  |_ -> "Not Valid"
;;

let rec rdcss d =
  match d with
  |DESC(RDCSS_DESC, st, (CAS (a1, o1, n1))::(CAS (a2, o2, n2))::_) ->
    print_endline (sprintf "TH%d: <<< RDCSS (ref %d) %s" (Domain.self ()) (a2.id) (rdcss_str d));
    (* let d' = Gc.promote_to d a2 in *)
    let r = commit (CAS(a2, o2, d)) in
    print_endline (sprintf "TH%d: !!! r = %s !!!" (Domain.self ()) (word r));
    print_endline (sprintf "TH%d: >>> RDCSS (ref %d) (r = %s (tag %d)) %s" (Domain.self ()) (a2.id) (word r) (Obj.tag (Obj.repr r)) (rdcss_str d));
    print_endline (sprintf "TH%d: ??? r = %s ???" (Domain.self ()) (word r));
    if is_descriptor r then begin
      print_endline (sprintf "TH%d: RDCSS OTHER DESC (ref %d) (r = %s)" (Domain.self ()) (a2.id) (word r));
      complete r; rdcss d
    end else begin
      if r = o2 then
        (print_endline (sprintf "TH%d: RDCSS COMPLETE (ref %d) (r = %s = %s = o)" (Domain.self ()) (a2.id) (word r) (word o2));
         complete d; ())
      else
        print_endline (sprintf "TH%d: RDCSS FAIL (ref %d) (r = %s != %s = o)" (Domain.self ()) (a2.id) (word r) (word o2));
      r
    end
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





let rec rdcss_bis d =
  match d with
  |DESC(RDCSS_DESC, st, (CAS (a1, o1, n1))::(CAS (a2, o2, n2))::_) ->
    (* print_endline (sprintf "TH%d: <<< RDCSS (ref %d) %s" (Domain.self ()) (a2.id) (rdcss_str d)); *)
    (* let d' = Gc.promote_to d a2 in *)
    if cas1_bis a2 o2 d then
      (* (print_endline (sprintf "TH%d: RDCSS COMPLETE (ref %d) (r = %s = %s = o)" (Domain.self ()) (a2.id) (get a2) (word o2)); *)
       (complete_bis d; o2)
    else
      let r = a2.content in
      (* print_endline (sprintf "TH%d: SWAP FAILED (ref %d) (r = %s)" (Domain.self ()) (a2.id) (word r)); *)
      if is_descriptor r then
        (* (print_endline (sprintf "TH%d: RDCSS OTHER DESC (ref %d) (r = %s)" (Domain.self ()) (a2.id) (word r)); *)
        (complete_bis r; rdcss_bis d)
      else if r = o2 then
        (* (print_endline (sprintf "TH%d: RDCSS RETRY (ref %d) (r = %s)" (Domain.self ()) (a2.id) (word r)); *)
        (rdcss_bis d)
      else
        (* (print_endline (sprintf "TH%d: RDCSS FAILED (ref %d) (r = %s)" (Domain.self ()) (a2.id) (word r)); *)
        (r)
  | _ -> failwith "Rdcss Invalid Argument"
and complete_bis d =
  match d with
  |DESC(RDCSS_DESC, st, (CAS (a1, o1, n1))::(CAS (a2, o2, n2))::_) ->
    if a1.content = o1 then
      cas1_bis a2 d n2
    else
      cas1_bis a2 d o2
  | _ -> failwith "Complete Invalid Argument"
;;





let rec rdcss_read a =
  let r = a.content in
  if is_descriptor r then begin
    complete_bis r; rdcss_read a
  end else
    r
;;

(* UNDECIDED : 0    FAILED : 1    SUCCEEDED : 2 *)
let rec casn cd =
  match cd with
  |DESC(CASN_DESC, st, c_l) ->
    (* print_endline "GOOD ARGUMENT"; *)
    let rec phase1 c_l curr_st out =
      (* print_endline (sprintf "DEB PHASE1   %d" curr_st); *)
      match c_l with
      |(CAS(a, o, n))::c_t_tail when curr_st = 2 ->
        let s = rdcss_bis (mk_rdcss_desc (CAS(st, WORD(0), WORD(0))) (CAS(a, o, cd))) in
        if is_casn s && s != cd then begin
          (* print_endline (sprintf "TH%d: Already a CASN Descriptor (%s)!!!" (Domain.self ()) (get a)); *)
          casn s;
          (* print_endline (sprintf "TH%d: Already a CASN Descriptor (%s) BIS!!!" (Domain.self ()) (get a)); *)
          phase1 c_l curr_st out
        end else begin
          let new_curr_st = if is_casn s || s = o then curr_st else 1 in
          (* print_endline (sprintf "TH%d: OK RDCSS? %d (s = %s) (o = %s)"
          (Domain.self ()) new_curr_st (word s) (word o)); *)
          phase1 c_t_tail new_curr_st ((CAS(a, o, n))::out)
        end
      |_ -> (*print_endline "Phase 1 FIN";*) cas1_bis st (WORD(0)) (WORD(curr_st)); out
    in
    let rec phase2 c_l succ =
      match c_l with
      |(CAS(a, o, n))::c_t_tail -> begin
        (* print_endline "Depilement"; *)
        (match succ with
         |WORD(2) ->
           let out = cas1_bis a cd n in
           (* print_endline (sprintf "TH%d: Phase 2 SUCCESS %s" (Domain.self ()) (word out)); *)
           out
         |_ ->
           let out = cas1_bis a cd o in
           (* print_endline (sprintf "TH%d: Phase 2 FAILED %s" (Domain.self ()) (word out)); *)
           out);
        phase2 c_t_tail succ
      end
      |[] -> succ = WORD(2)
    in
    (* print_endline (sprintf "STATUS %s" (get st)); *)
    let cas_to_proceed =
      if st.content = WORD(0) then begin
        (* print_endline "BEGIN PHASE1"; *)
        phase1 c_l 2 []
      end else
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
    let WORD(out) = r in
    out
;;

(* let n_max = 1000;;

let rec thread1 (a1, a2) =
  let n = Pervasives.ref 0 in
  for i = 1 to n_max do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(0), WORD(1))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(1), WORD(0))]) in
    (* print_endline "CALL"; *)
    print_endline (sprintf "Appel thread%d n°%d  (%s, %s)\n\n" (Domain.self ()) i (get a1) (get a2));
    let out1 = casn cd1 in
    (* print_endline (sprintf "Thread 1,    out1 : %b,    val1 : %s,    val2 : %s\n" out1 (get a1) (get a2)); *)
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 1,    out2 : %b,    val1 : %s,    val2 : %s\n" out2 (get a1) (get a2)); *)
    if out1 = true && out2 = true then
      n := !n + 1
    else
      print_endline "ALERTE ECHEC !!!"
  done;
  print_endline (sprintf "Thread %d : %d/%d\n" (Domain.self ()) !n n_max)
;;

let rec thread2 (a1, a2) =
  let n = Pervasives.ref 0 in
  for i = 1 to n_max do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
    print_endline (sprintf "Appel thread%d n°%d  (%s, %s)\n\n" (Domain.self ()) i (get a1) (get a2));
    let out1 = casn cd1 in
    (* print_endline (sprintf "Thread 2,    out1 : %b,    val1 : %s,    val2 : %s\n" out1 (get a1) (get a2)); *)
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 2,    out2 : %b,    val1 : %s,    val2 : %s\n" out2 (get a1) (get a2)); *)
    if out1 = false && out2 = false then
      n := !n + 1
  done;
  print_endline (sprintf "Thread %d : %d/%d\n" (Domain.self ()) !n n_max)
;;

let rec thread3 (a1, a2) =
  let n = Pervasives.ref 0 in
  for i = 1 to n_max do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
    print_endline (sprintf "Appel thread%d n°%d  (%s, %s)\n\n" (Domain.self ()) i (get a1) (get a2));
    let out1 = casn cd1 in
    (* print_endline (sprintf "Thread 3,    out1 : %b,    val1 : %s,    val2 : %s\n" out1 (get a1) (get a2)); *)
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 3,    out2 : %b,    val1 : %s,    val2 : %s\n" out2 (get a1) (get a2)); *)
    if out1 = false && out2 = false then
      n := !n + 1
  done;
  print_endline (sprintf "Thread %d : %d/%d\n" (Domain.self ()) !n n_max)
;; *)

(*
let () =
  let a1 = ref 0 in
  let a2 = ref 0 in

  let computation1 () = thread1 (a1, a2) in
(* WORKS *)
  (* thread1 (a1, a2); *)

(* DOESN'T WORK *)
  Domain.spawn (fun () -> thread1 (a1, a2));
  Domain.spawn (fun () -> thread2 (a1, a2));
  (* Domain.spawn (fun () -> thread3 (a1, a2)); *)

  Unix.sleep 18

;; *)












(*
####
*)
