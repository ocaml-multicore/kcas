#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "kcas" @@ fun c ->
  Ok [ Pkg.mllib "src/kcas.mllib";
       Pkg.test "test/test";
       Pkg.test "test/benchmark"; ]
