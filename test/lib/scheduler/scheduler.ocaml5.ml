let () =
  Random.self_init ();
  Picos_io_select.configure ()

let run ?(n_domains = 1) main =
  if Random.bool () then Picos_mux_multififo.run_on ~quota:100 ~n_domains main
  else Picos_mux_random.run_on ~n_domains main
