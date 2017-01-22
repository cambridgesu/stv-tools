open OUnit

let suite =
  "test stv" >:::
    [
      "test line of numbers" >:: Test_line_of_numbers.test_lon_sane;
      "test line of numbers ends" >:: Test_line_of_numbers.test_lon_ends;
      "test line of broken numbers" >:: Test_line_of_numbers.test_lon_broken;
      "test normal blt load" >:: Test_blt.test_load_normal;
    ]

let run () = run_test_tt_main suite

let _ = 
  let anon s = Printf.printf "Unhandled argument: %s\n" s; exit 2 in
  let usage = "test_stv [--dir directory]" in
    Arg.parse [("--dir", Arg.Set_string Test_blt.blt_dir, 
                "Directory containing BLT test files")] anon usage;
    run ()

