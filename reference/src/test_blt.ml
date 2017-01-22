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
