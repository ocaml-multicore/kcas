let run ~verbose ~count ~name (module Spec : STM.Spec) =
  let module Seq = STM_sequential.Make (Spec) in
  let module Con = STM_thread.Make (Spec) [@alert "-experimental"] in
  QCheck_base_runner.run_tests ~verbose
    [
      Seq.agree_test ~count ~name:(name ^ " sequential");
      Con.agree_test_conc ~count ~name:(name ^ " concurrent");
    ]
