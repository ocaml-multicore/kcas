let () =
  Random.self_init ();
  Picos_io_select.configure ()

let run ?n_domains:_ main = Picos_mux_thread.run main
