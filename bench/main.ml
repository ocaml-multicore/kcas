let benchmarks =
  [
    ("Atomic", Bench_atomic.run_suite);
    ("Kcas Loc", Bench_loc.run_suite);
    ("Kcas Xt", Bench_xt.run_suite);
    ("Kcas parallel CMP", Bench_parallel_cmp.run_suite);
    ("Kcas_data Accumulator", Bench_accumulator.run_suite);
    ("Kcas_data Hashtbl", Bench_hashtbl.run_suite);
    ("Kcas_data Mvar", Bench_mvar.run_suite);
    ("Kcas_data Queue", Bench_queue.run_suite);
    ("Kcas_data Stack", Bench_stack.run_suite);
  ]

let rec replace_inf : Yojson.Safe.t -> Yojson.Safe.t = function
  | `Float x as lit -> if Float.is_finite x then lit else `Float 0.0
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `String _) as lit -> lit
  | `Assoc kvs -> `Assoc (List.map (fun (k, v) -> (k, replace_inf v)) kvs)
  | `List vs -> `List (List.map replace_inf vs)
  | `Tuple vs -> `Tuple (List.map replace_inf vs)
  | `Variant (k, vo) -> `Variant (k, Option.map replace_inf vo)

let () =
  let factor = try int_of_string Sys.argv.(1) with _ -> 1 in

  let benchmarks = benchmarks in

  let run (name, fn) =
    Gc.full_major ();
    let metrics = fn ~factor in
    `Assoc [ ("name", `String name); ("metrics", `List metrics) ]
  in

  `Assoc [ ("results", `List (List.map run benchmarks)) ]
  |> replace_inf
  |> Yojson.Safe.pretty_print ~std:true Format.std_formatter
