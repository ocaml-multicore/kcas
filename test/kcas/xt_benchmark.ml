open Printf
open Kcas

let k_kCAS, num_iter =
  if Array.length Sys.argv < 3 then (2, 1000)
  else
    try
      let a = int_of_string Sys.argv.(1) in
      let b = int_of_string Sys.argv.(2) in
      (a, b)
    with Failure _ -> failwith "Unable to parse arguments"

let make_kCAS k =
  let rs = List.init k (fun _ -> Loc.make 0) in
  let rec incs ~xt = function
    | [] -> failwith "bug"
    | [ r ] -> Xt.modify ~xt r (( + ) 1)
    | r :: rs ->
        Xt.modify ~xt r (( + ) 1);
        incs ~xt rs
  in
  let rec decs ~xt = function
    | [] -> failwith "bug"
    | [ r ] -> Xt.modify ~xt r (( + ) (-1))
    | r :: rs ->
        Xt.modify ~xt r (( + ) (-1));
        decs ~xt rs
  in
  ({ Xt.tx = incs rs }, { Xt.tx = decs rs })

let operation1, operation2 = make_kCAS k_kCAS

module Benchmark = struct
  let get_mean_sd l =
    let get_mean l =
      List.fold_right (fun a v -> a +. v) l 0. /. (float_of_int @@ List.length l)
    in
    let mean = get_mean l in
    let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
    (mean, sd)

  let benchmark f n =
    let rec run acc = function
      | 0 -> acc
      | n ->
          let t1 = Unix.gettimeofday () in
          let () = f () in
          let d = Unix.gettimeofday () -. t1 in
          run (d :: acc) (n - 1)
    in
    let r = run [] n in
    get_mean_sd r
end

let benchmark () =
  let rec loop i =
    if i > 0 then (
      Xt.commit operation1;
      Xt.commit operation2;
      loop (i - 1))
  in
  loop num_iter

let main () =
  let n = 10 in
  let m, _ = Benchmark.benchmark benchmark n in
  print_endline (sprintf "%f" m)

let () = main ()
