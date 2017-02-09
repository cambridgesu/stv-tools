(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open OUnit

let suite =
  "test stv" >:::
    [
      "test line of numbers" >:: Test_line_of_numbers.test_lon_sane;
      "test line of numbers ends" >:: Test_line_of_numbers.test_lon_ends;
      "test line of broken numbers" >:: Test_line_of_numbers.test_lon_broken;
      "test normal blt load" >:: Test_blt.test_load_normal;
      "test broken blt loads" >:: Test_blt.test_load_broken;
      "test known good blts" >:: Test_blt.test_load_good_samples;
    ]

let run () = run_test_tt_main suite

let _ =
  let anon s = Printf.printf "Unhandled argument: %s\n" s; exit 2 in
  let usage = "test_stv [--dir directory]" in
    Arg.parse [("--dir", Arg.Set_string Test_blt.blt_dir,
                "Directory containing BLT test files")] anon usage;
    run ()
