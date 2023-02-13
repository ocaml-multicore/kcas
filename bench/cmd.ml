let () =
  let bench_name, num_iter, num_ops =
    if Array.length Sys.argv < 4 then (`Core, 10_000, 5)
    else
      try
        let a =
          match String.lowercase_ascii Sys.argv.(1) with
          | "core" -> `Core
          | "tx" -> `Tx
          | s -> failwith ("unknown benchmark: " ^ s)
        in
        let b = int_of_string Sys.argv.(2) in
        let c = int_of_string Sys.argv.(3) in
        (a, b, c)
      with Failure s ->
        failwith
          ("Unable to parse arguments [" ^ s
         ^ "]. Usage: cmd.exe [core|tx] iterations operations_per_kcas")
  in
  let `Mean mean, `Median median, `Stddev stddev =
    match bench_name with
    | `Core -> Core_bench.run ~num_iter ~num_ops ()
    | `Tx -> Tx_bench.run ~num_iter ~num_ops ()
  in
  Printf.printf "median: %f, mean: %f, stddev: %f\n%!" mean median stddev
