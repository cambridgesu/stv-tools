open OUnit

let test_x () =
  assert_equal true false

let suite =
  "test stv" >:::
    [
      "test thing" >:: test_x
    ]

let run () = run_test_tt_main suite

let _ = 
  let blt_dir = ref "." in
  let anon s = Printf.printf "Unhandled argument: %s\n" s; exit 2 in
  let usage = "test_stv [--dir directory]" in
    Arg.parse [("--dir", Arg.Set_string blt_dir, 
                "Directory containing BLT test files")] anon usage;
    run ()

