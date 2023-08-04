# Scheduler interop

The blocking mechanism in **kcas** is based on a
[_domain local await_](https://github.com/ocaml-multicore/domain-local-await)
mechanism that schedulers can choose to implement to allow libraries like
**kcas** to work with them.

Implementing schedulers is not really what casual users of **kcas** are supposed
to do. Below is an example of a _toy_ scheduler whose purpose is only to give a
sketch of how a scheduler can provide the domain local await mechanism.

Let's also demonstrate the use of the
[`Queue`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Queue/index.html),
[`Stack`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Stack/index.html),
and
[`Promise`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Promise/index.html)
implementations that are conveniently provided by the
[**kcas_data**](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/index.html)
package.

<!--
```ocaml
# #thread
# #require "kcas_data"
# open Kcas_data
# open Kcas
```
-->

Here is the full toy scheduler module:

```ocaml
module Scheduler : sig
  type t
  val spawn : unit -> t
  val join : t -> unit
  val fiber : t -> (unit -> 'a) -> 'a Promise.t
end = struct
  open Effect.Deep
  type _ Effect.t +=
    | Suspend : (('a, unit) continuation -> unit) -> 'a Effect.t
  type t = {
    queue: (unit -> unit) Queue.t;
    domain: unit Domain.t
  }
  let spawn () =
    let queue = Queue.create () in
    let rec scheduler work =
      let effc (type a) : a Effect.t -> _ = function
        | Suspend ef -> Some ef
        | _ -> None in
      try_with work () { effc };
      match Queue.take_opt queue with
      | Some work -> scheduler work
      | None -> () in
    let prepare_for_await _ =
      let state = Atomic.make `Init in
      let release () =
        if Atomic.get state != `Released then
          match Atomic.exchange state `Released with
          | `Awaiting k ->
            Queue.add (continue k) queue
          | _ -> () in
      let await () =
        if Atomic.get state != `Released then
          Effect.perform @@ Suspend (fun k ->
            if not (Atomic.compare_and_set state `Init
                      (`Awaiting k)) then
              continue k ())
      in
      Domain_local_await.{ release; await } in
    let domain = Domain.spawn @@ fun () ->
      try
        while true do
          let work = Queue.take_blocking queue in
          Domain_local_await.using
            ~prepare_for_await
            ~while_running:(fun () -> scheduler work)
        done
      with Exit -> () in
    { queue; domain }
  let join t =
    Queue.add (fun () -> raise Exit) t.queue;
    Domain.join t.domain
  let fiber t thunk =
    let (promise, resolver) = Promise.create () in
    Queue.add
      (fun () -> Promise.resolve resolver (thunk ()))
      t.queue;
    promise
end
```

The idea is that one can spawn a scheduler to run on a new domain. Then one can
run fibers on the scheduler. Because the scheduler provides the domain local
await mechanism libraries like **kcas** can use it to block in a scheduler
independent and friendly manner.

Let's then demonstrate the integration. To start we spawn a scheduler:

```ocaml
# let scheduler = Scheduler.spawn ()
val scheduler : Scheduler.t = <abstr>
```

The scheduler is now eagerly awaiting for fibers to run. Let's give it a couple
of them, but, let's first create a queue and a stack to communicate with the
fibers:

```ocaml
# let in_queue : int Queue.t = Queue.create ()
val in_queue : int Kcas_data.Queue.t = <abstr>
# let out_stack : int Stack.t = Stack.create ()
val out_stack : int Kcas_data.Stack.t = <abstr>
```

The first fiber we create just copies elements from the `in_queue` to the
`out_stack`:

```ocaml
# ignore @@ Scheduler.fiber scheduler @@ fun () ->
    while true do
      let elem = Queue.take_blocking in_queue in
      Printf.printf "Giving %d...\n%!" elem;
      Stack.push elem out_stack
    done
- : unit = ()
```

The second fiber awaits to take two elements from the `out_stack`, updates a
state in between, and then returns their sum:

```ocaml
# let state = Loc.make 0
val state : int Loc.t = <abstr>
# let sync_to target =
    state
    |> Loc.get_as @@ fun current ->
       Retry.unless (target <= current)
val sync_to : int -> unit = <fun>
# let a_promise = Scheduler.fiber scheduler @@ fun () ->
    let x = Stack.pop_blocking out_stack in
    Printf.printf "First you gave me %d.\n%!" x;
    Loc.set state 1;
    let y = Stack.pop_blocking out_stack in
    Printf.printf "Then you gave me %d.\n%!" y;
    Loc.set state 2;
    x + y
val a_promise : int Promise.t = <abstr>
```

To interact with the fibers, we add some elements to the `in_queue`:

```ocaml
# Queue.add 14 in_queue; sync_to 1
Giving 14...
First you gave me 14.
- : unit = ()
# Queue.add 28 in_queue; sync_to 2
Giving 28...
Then you gave me 28.
- : unit = ()
# Promise.await a_promise
- : int = 42
```

As can be seen above, the scheduler multiplexes the domain among the fibers.
Notice that thanks to the domain local await mechanism we could just perform
blocking operations without thinking about the schedulers. Communication between
the main domain, the scheduler domain, and the fibers _just works_ ™.

Time to close the shop.

```ocaml
# Scheduler.join scheduler
- : unit = ()
```

_That's all Folks!_
