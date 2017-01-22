open Line_of_numbers
open OUnit

let test_lon_ends () =
  let src = "1 2 3 4" in
  let obs = Line_of_numbers.safe_array src in
    assert_equal [|1; 2; 3; 4|] obs;

  let obs' = Line_of_numbers.list_without_ends obs in
    assert_equal [2; 3] obs'

let test_lon_broken () =
  let src = "123 024 00 " in
    assert_raises (Invalid_line_of_numbers)
      (fun () -> Line_of_numbers.safe_array src);

  let src = "-1 0" in
  let obs = Line_of_numbers.safe_array src in
    assert_equal [|-1; 0|] obs;

  let src = "9 9" in
  let obs = Line_of_numbers.safe_array src in
    assert_equal [|9; 9|] obs

let test_lon_sane () =
  let exp_src = [
    ([|1; 2; 3; 4|], "1 2 3 4");
    ([|123; 24; 0|], "123 024 00");
    ([|-1; 0|], "-1 0");
    ([|9; 9|], "9 9");
  ]
  in
    List.iter (fun (exp, src) ->
      Line_of_numbers.safe_array src |> assert_equal exp) exp_src
