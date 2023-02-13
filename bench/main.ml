let output results =
  let results = String.concat "," results in
  (* Avoiding rewriters to be able to run these benchmarks against trunk. *)
  Printf.sprintf {| {"name": "kcas", "results": [%s]}|} results

let () =
  let results =
    [
      Core_bench.run ~num_iter:1_000_000 ~num_ops:2 ()
      |> Benchmark.as_json "core-kcas-two-op";
      Tx_bench.run ~num_iter:1_000_000 ~num_ops:2 ()
      |> Benchmark.as_json "tx-two-op";
      Core_bench.run ~num_iter:100_000 ~num_ops:10 ()
      |> Benchmark.as_json "core-kcas-ten-op";
      Tx_bench.run ~num_iter:100_000 ~num_ops:10 ()
      |> Benchmark.as_json "tx-ten-op";
    ]
  in
  print_string (output results)
