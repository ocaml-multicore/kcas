
open Printf;;

let print_usage_and_exit () =
  print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <k> <num_iter>";
  exit(0)
;;

let (k_kCAS, num_iter) =
  if Array.length Sys.argv < 3 then
    print_usage_and_exit ()
  else try
    let a = int_of_string (Sys.argv.(1)) in
    let b = int_of_string (Sys.argv.(2)) in
    (a,b)
  with Failure _ -> print_usage_and_exit ()
;;

let make_kCAS k =
  let rec loop k out1 out2 =
    if k > 0 then
      let a = Kcas.ref 0 in
      loop (k-1) ((Kcas.mk_cas a 0 1)::out1) ((Kcas.mk_cas a 1 0)::out2)
    else
      (out1, out2)
  in loop k [] []
;;

let operation1, operation2 = make_kCAS k_kCAS;;


module Benchmark = struct
  let get_mean_sd l =
    let get_mean l = (List.fold_right (fun a v -> a +. v) l 0.) /. (float_of_int @@ List.length l) in
    let mean = get_mean l in
    let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
    (mean, sd)
  ;;

  let benchmark f n =
    let rec run acc = function
    |0 -> acc
    |n ->
      let t1 = Unix.gettimeofday () in
      let () = f () in
      let d = Unix.gettimeofday () -. t1 in
      run (d::acc) (n-1)
    in
    let r = run [] n in
    get_mean_sd r
  ;;
end;;

let benchmark () =
  let rec loop i =
    if i > 0 then begin
      ignore @@ Kcas.kCAS operation1;
      ignore @@ Kcas.kCAS operation2;
      loop (i-1)
    end
  in loop num_iter
;;

let main () =
  let n = 10 in
  let (m,sd) = Benchmark.benchmark benchmark n in
  print_endline (sprintf "%f" m)
;;

let () = main ();;
