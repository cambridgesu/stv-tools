open OUnit

let test_x () =
  assert_equal true false

let suite =
  "test stv" >:::
    [
      "test thing" >:: test_x
    ]

let run () = run_test_tt_main suite

let _ = run ()
