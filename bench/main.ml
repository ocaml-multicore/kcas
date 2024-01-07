let benchmarks =
  [
    ("Ref with [@poll error]", Bench_ref.run_suite);
    ("Atomic", Bench_atomic.run_suite);
    ("Kcas Loc", Bench_loc.run_suite);
    ("Kcas Xt", Bench_xt.run_suite);
    ("Kcas Xt read-only", Bench_xt_ro.run_suite);
    ("Kcas parallel CMP", Bench_parallel_cmp.run_suite);
    ("Kcas_data Accumulator", Bench_accumulator.run_suite);
    ("Kcas_data Dllist", Bench_dllist.run_suite);
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
  let budgetf = ref 0.025 in
  let filters = ref [] in

  let rec specs =
    [
      ("-budget", Arg.Set_float budgetf, "seconds\t  Budget for a benchmark");
      ("-help", Unit help, "\t  Show this help message");
      ("--help", Unit help, "\t  Show this help message");
    ]
  and help () =
    Arg.usage (Arg.align specs)
      (Printf.sprintf
         "\n\
          Usage: %s <option>* filter*\n\n\
          The filters are regular expressions for selecting benchmarks to run.\n\n\
          Benchmarks:\n\n\
          %s\n\n\
          Options:\n"
         (Filename.basename Sys.argv.(0))
         (benchmarks
         |> List.map (fun (name, _) -> "  " ^ name)
         |> String.concat "\n"));
    exit 1
  in
  Arg.parse specs (fun filter -> filters := filter :: !filters) "";

  let budgetf = !budgetf in

  let run (name, fn) =
    let metrics = fn ~budgetf in
    `Assoc [ ("name", `String name); ("metrics", `List metrics) ]
  in

  let filter =
    match !filters with
    | [] -> Fun.const true
    | filters -> (
        let regexps = filters |> List.map Str.regexp in
        fun (name, _) ->
          regexps
          |> List.exists @@ fun regexp ->
             match Str.search_forward regexp name 0 with
             | _ -> true
             | exception Not_found -> false)
  in

  `Assoc
    [ ("results", `List (benchmarks |> List.filter filter |> List.map run)) ]
  |> replace_inf
  |> Yojson.Safe.pretty_print ~std:true Format.std_formatter
