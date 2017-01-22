(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Blt
open OUnit

let blt_dir = ref "."

let get_file filename =
  !blt_dir ^ "/" ^ filename |> open_in

let load_file filename =
  get_file filename |>
  Blt.tally_of_blt_stream |>
  ignore

let test_load_normal () =
  load_file "example.blt" |>
  assert_equal ()

let test_load_broken () =
  [
    (Invalid_header, "invalid-header.blt");
  ] |>
      List.iter (fun (exp, src) ->
        assert_raises exp (fun () -> load_file src))
