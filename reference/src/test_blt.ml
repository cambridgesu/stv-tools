open Blt
open OUnit

let blt_dir = ref "."

let get_file filename =
  !blt_dir ^ "/" ^ filename |> open_in

let test_load_normal () =
  get_file "example.blt" |>
  Blt.tally_of_blt_stream |>
  ignore |>
  assert_equal ()
