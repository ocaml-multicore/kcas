(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 * Copyright (c) 2021, Sudha Parimala <sudharg247@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  type t

  val create : ?max:int -> unit -> t

  val once : t -> unit

  val reset : t -> unit
end

module M : S = struct
  type t = int * int ref

  let _ = Random.self_init ()

  let create ?(max = 32) () = (max, ref 1)

  let once (maxv, r) =
    let t = Random.int !r in
    r := min (2 * !r) maxv ;
    if t = 0 then ()
    else begin
      for _ = 1 to 2048 * t do
        Domain.Sync.cpu_relax ()
      done
    end

  let reset (_, r) = r := 1
end
