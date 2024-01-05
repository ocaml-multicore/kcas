let benchmarks =
  [
    ("Ref with [@poll error]", Bench_ref.run_suite);
    ("Ref with Mutex", Bench_ref_mutex.run_suite);
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

let () = Multicore_bench.Cmd.run ~benchmarks ()
