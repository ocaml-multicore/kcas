let cpu_relax = Thread.yield
let recommended_domain_count () = 1

type 'a t = { result : ('a, exn) Result.t ref; thread : Thread.t }

let spawn fn =
  let result = ref @@ Error Exit in
  let thread =
    ()
    |> Thread.create @@ fun () ->
       match fn () with
       | value -> result := Ok value
       | exception exn -> result := Error exn
  in
  { result; thread }

let join t =
  Thread.join t.thread;
  match !(t.result) with Ok value -> value | Error exn -> raise exn
