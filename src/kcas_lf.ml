(*

Compilation : ocamlc -thread unix.cma threads.cma essaie.ml

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

let compare_and_swap r x y =
  (Obj.compare_and_swap_field (Obj.repr r) 0 (Obj.repr x) (Obj.repr y))
;;

let ref x = {
  content = WORD(x);
  id = Oo.id (object end);
};;

let word w =
  match w with
  |WORD(a) -> sprintf "%d" a
  |_ -> "ASSERT"
  (* |_ -> assert false *)
;;

let get r = word r.content;;

let mk_rdcss_desc c1 c2 =
  DESC(RDCSS_DESC, ref 0, [c1 ; c2])
;;

let cas r expect update =
  match r.content, expect with
  |WORD(x), WORD(e) when x == e ->
    if expect == update then
      true
    else
      compare_and_swap r r.content update
  |DESC(d1, s1, l1), DESC(d2, s2, l2) when d1 == d2 && s1 == s2 && l1 == l2 ->
    compare_and_swap r r.content update
  |_ -> false
;;

let commit (CAS (r, expect, update)) =
  cas r expect update
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
    let curr_cas = CAS (a2, o2, d) in
    if commit curr_cas then begin
      (* print_endline "Rdcss Branche 1"; *)
      complete a2.content
    end else if is_descriptor a2.content then begin
      (* print_endline "Rdcss Branche 2"; *)
      complete a2.content;
      rdcss d
    end else begin
      (* print_endline "Rdcss Branche 3"; *)
      false
    end
  |_ -> false
and complete d =
  match d with
  |DESC(RDCSS_DESC, st, (CAS (a1, o1, n1))::(CAS (a2, o2, n2))::_) ->
    if a1.content = o1 then begin
      (* print_endline "Complete Branche 1"; *)
      let out = commit (CAS (a2, d, n2)) in
      (* printf "Out = %b\n" out; *)
      out
    end else begin
      (* print_endline "Complete Branche 2"; *)
      commit (CAS (a2, d, o2)); false
    end
  |_ -> false
;;

let rec rdcss_read a =
  if is_descriptor a.content then
    (complete a.content; ());
  if is_descriptor a.content then
    rdcss_read a
  else
    ((*print_string "ICI\n";*) a.content)
;;

(* UNDECIDED : 0    FAILED : 1    SUCCEEDED : 2 *)
let rec casn cd =
  match cd with
  |DESC(CASN_DESC, st, c_l) ->
    let rec phase1 c_l curr_st =
      match c_l with
      |(CAS(a, o, n))::c_t_tail when curr_st = 2 ->
        (* print_endline (sprintf "PHOTO ----- %s" (get_st st.content)); *)
        rdcss (mk_rdcss_desc (CAS(st, WORD(0), WORD(0))) (CAS(a, o, cd)));
        if is_casn a.content then begin
          print_endline "Phase 1 Branche 1";
          if a.content != cd then begin
            print_endline "Phase 1 Branche 1.1";
            casn a.content;
            phase1 c_l curr_st
          end
        end else if a.content != o then begin
          print_endline "Phase 1 Branche 2";
          phase1 c_l 1
        end;
        print_endline "Phase 1 Branche 3";
        phase1 c_t_tail curr_st
      |_ -> (*print_endline "Phase 1 FIN";*) cas st (WORD(0)) (WORD(curr_st)); ()
    in
    let rec phase2 c_l succ =
      match c_l with
      |(CAS(a, o, n))::c_t_tail -> begin
        (* print_endline "Depilement"; *)
        (match succ with
         |WORD(2) ->
           print_endline "Phase 2 Branche 1";
           cas a cd n
         |_ ->
           print_endline "Phase 2 Branche 2";
           cas a cd o);
        phase2 c_t_tail succ
      end
      |[] -> succ = WORD(2)
    in
    (* print_endline (sprintf "BEGIN    %s" (get_st st.content)); *)
    if st.content = WORD(0) then
      phase1 c_l 2;
    phase2 c_l st.content
  |_ -> false
;;

let rec casn_read a =
  let r = rdcss_read a in
  if is_casn r then begin
    casn r;
    casn_read a
  end else
    r
;;

let rec thread1 (a1, a2) =
  let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(0), WORD(1))]) in
  let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(1), WORD(0))]) in
  let rec loop n =
    print_endline (sprintf "Appel thread1 n°%d\n\n" n);
    let out1 = casn cd1 in
    (* print_endline (sprintf "Thread 1,    out1 : %b\n" out1); *)
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 1,    out2 : %b\n" out2);
    printf "FIN thread1\n\n"; *)
    loop (n+1)
  in
  try
    loop 0
  with e ->
    let rec tmp () =
      print_endline (sprintf "ALERTE     %s  !!!" (Printexc.to_string e)); tmp ()
    in
    tmp ()
;;

let rec thread2 (a1, a2) =
  let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
  let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
  let rec loop n =
    let out1 = casn cd1 in
    print_endline (sprintf "Thread 2,    out1 : %b\n" out1);
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 2,    out2 : %b\n" out2);
    printf "FIN thread2\n\n"; *)
    loop (n+1)
  in
  loop 0
;;

let rec thread3 (a1, a2) =
  let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
  let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
  let rec loop n =
    let out1 = casn cd1 in
    print_endline (sprintf "Thread 3,    out1 : %b\n" out1);
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 3,    out2 : %b\n" out2); *)
    (* printf "FIN thread3\n\n"; *)
    loop (n+1)
  in
  loop 0
;;


let () =
  let a1 = ref 0 in
  let a2 = ref 0 in
  let compt_good = ref 0 in
  let compt_good = ref 0 in

  let th1 = Thread.create thread1 (a1, a2) in
  (* let th2 = Thread.create thread2 (a1, a2) in
  let th3 = Thread.create thread3 (a1, a2) in *)

  Unix.sleep 10
;;












(*
####
*)
