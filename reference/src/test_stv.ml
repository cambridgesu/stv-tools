open OUnit

let blt_dir = ref "."

let get_file filename =
  let path = !blt_dir ^ "/" ^ filename in
    open_in path

let test_lon () =
  let src = "1 2 3 4" in
  let obs = Line_of_numbers.safe_array src in
    assert_equal [|1; 2; 3; 4|] obs;

  let obs' = Line_of_numbers.list_without_ends obs in
    assert_equal [2; 3] obs'

let suite =
  "test stv" >:::
    [
      "test line of numbers" >:: test_lon;
    ]

let run () = run_test_tt_main suite

let _ = 
  let anon s = Printf.printf "Unhandled argument: %s\n" s; exit 2 in
  let usage = "test_stv [--dir directory]" in
    Arg.parse [("--dir", Arg.Set_string blt_dir, 
                "Directory containing BLT test files")] anon usage;
    run ()

