let run ~verbose ~count ~name (module Spec : STM.Spec) =
  let module Seq = STM_sequential.Make (Spec) in
  let module Dom = STM_domain.Make (Spec) in
  QCheck_base_runner.run_tests ~verbose
    [
      Seq.agree_test ~count ~name:(name ^ " sequential");
      Dom.agree_test_par ~count ~name:(name ^ " parallel");
    ]
