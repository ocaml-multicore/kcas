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
  |_ -> "DESC string"
  (* |_ -> assert false *)
;;

let get r = word r.content;;

let mk_rdcss_desc c1 c2 =
  DESC(RDCSS_DESC, ref 0, [c1 ; c2])
;;

let cas r expect update =
  let s = r.content in
  match s, expect with
  |WORD(x), WORD(e) when x == e ->
    (* print_endline "CAS WORD"; *)
    if expect == update then
      true
    else
      compare_and_swap r s update
  |DESC(d1, s1, l1), DESC(d2, s2, l2) when d1 == d2 && s1 == s2 && l1 == l2 ->
    (* print_endline "CAS DESC"; *)
    compare_and_swap r s update
  |_ -> (*print_endline "CAS INVALID ARGUMENT";*) false
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
    (* printf "(a1: %s, o1: %s, a2: %s, o2: %s, n2: %s)\n" (get a1)(word o1)(get a2)(word o2)(word n2); *)
    (* printf "(a2: %s, o2: %s, n2: %s)\n" (get a2)(word o2)(word n2); *)
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
    (* printf "ICI %b\n" (a1.content = o1); *)
    if a1.content = o1 then begin
      (* print_endline "Complete Branche 1"; *)
      let out = commit (CAS (a2, d, n2)) in
      (* printf "Commit Out = %b\n" out; *)
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
    (* print_endline "GOOD ARGUMENT"; *)
    let rec phase1 c_l curr_st =
      (* print_endline (sprintf "DEB PHASE1   %d" curr_st); *)
      match c_l with
      |(CAS(a, o, n))::c_t_tail when curr_st = 2 ->
        let out_rdcss = rdcss (mk_rdcss_desc (CAS(st, WORD(0), WORD(0))) (CAS(a, o, cd))) in
        (* print_endline (sprintf "CASN rdcss output : %b" out_rdcss); *)
        if is_casn a.content then begin
          (* print_endline "Phase 1 Branche 1"; *)
          if a.content != cd then begin
            (* print_endline "Phase 1 Branche 1.1"; *)
            casn a.content;
            phase1 c_l curr_st
          end
        end else if a.content != o then begin
          (* print_endline "Phase 1 Branche 2"; *)
          phase1 c_l 1
        end;
        (* print_endline (sprintf "Phase 1 Branche 3  %d" curr_st); *)
        phase1 c_t_tail curr_st
      |_ -> (*print_endline "Phase 1 FIN";*) cas st (WORD(0)) (WORD(curr_st)); ()
    in
    let rec phase2 c_l succ =
      match c_l with
      |(CAS(a, o, n))::c_t_tail -> begin
        (* print_endline "Depilement"; *)
        (match succ with
         |WORD(2) ->
           (* print_endline "Phase 2 Branche 1"; *)
           cas a cd n
         |_ ->
           (* print_endline "Phase 2 Branche 2"; *)
           cas a cd o);
        phase2 c_t_tail succ
      end
      |[] -> succ = WORD(2)
    in
    (* print_endline (sprintf "STATUS %s" (get st)); *)
    if st.content = WORD(0) then begin
      (* print_endline "BEGIN PHASE1"; *)
      phase1 c_l 2;
    end;
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
  let n = Pervasives.ref 0 in
  let n_max = 10000 in
  for i = 1 to n_max do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(0), WORD(1))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(1), WORD(0))]) in
    (* print_endline "CALL"; *)
    (* print_endline (sprintf "Appel thread1 n°%d\n\n" !n); *)
    let out1 = casn cd1 in
    (* print_endline (sprintf "Thread 1,    out1 : %b,    val1 : %s,    val2 : %s\n" out1 (get a1) (get a2)); *)
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 1,    out2 : %b,    val1 : %s,    val2 : %s\n" out2 (get a1) (get a2)); *)
    if out1 = true && out2 = true then
      n := !n + 1
  done;
  print_endline (sprintf "Thread 1 : %d/%d\n" !n n_max)
;;

let rec thread2 (a1, a2) =
  let n = Pervasives.ref 0 in
  let n_max = 10000 in
  for i = 1 to n_max do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
    print_endline (sprintf "Appel thread2 n°%d\n\n" !n);
    let out1 = casn cd1 in
    (* print_endline (sprintf "Thread 2,    out1 : %b,    val1 : %s,    val2 : %s\n" out1 (get a1) (get a2)); *)
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 2,    out2 : %b,    val1 : %s,    val2 : %s\n" out2 (get a1) (get a2)); *)
    if out1 = false && out2 = false then
      n := !n + 1
  done;
  printf "Thread 2 : %d/%d\n" !n n_max
;;

let rec thread3 (a1, a2) =
  let n = Pervasives.ref 0 in
  let n_max = 10000 in
  for i = 1 to n_max do
    let cd1 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(0), WORD(1)) ; CAS (a2, WORD(1), WORD(0))]) in
    let cd2 = DESC(CASN_DESC, ref 0, [CAS (a1, WORD(1), WORD(0)) ; CAS (a2, WORD(0), WORD(1))]) in
    (* print_endline (sprintf "Appel thread3 n°%d\n\n" !n); *)
    let out1 = casn cd1 in
    (* print_endline (sprintf "Thread 3,    out1 : %b,    val1 : %s,    val2 : %s\n" out1 (get a1) (get a2)); *)
    let out2 = casn cd2 in
    (* print_endline (sprintf "Thread 3,    out2 : %b,    val1 : %s,    val2 : %s\n" out2 (get a1) (get a2)); *)
    if out1 = false && out2 = false then
      n := !n + 1
  done;
  printf "Thread 3 : %d/%d\n" !n n_max
;;


let () =
  let a1 = ref 0 in
  let a2 = ref 0 in
  
(* WORKS *)
  thread1 (a1, a2);

(* DOESN'T WORK *)
  let th1 = Thread.create thread1 (a1, a2) in
  Thread.join th1
;;












(*
####
*)
