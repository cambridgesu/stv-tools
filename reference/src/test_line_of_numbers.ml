open Line_of_numbers
open OUnit

let test_lon_ends () =
  let src = "1 2 3 4" in
  let obs = Line_of_numbers.safe_array src in
    assert_equal [|1; 2; 3; 4|] obs;

  let obs' = Line_of_numbers.list_without_ends obs in
    assert_equal [2; 3] obs'

let test_lon_broken () =
  [
    (Invalid_line_of_numbers, "123 024 00 ");
  ] |>
      List.iter (fun (exp, src) ->
        assert_raises exp (fun () -> Line_of_numbers.safe_array src))

let test_lon_sane () =
  [
    ([|1; 2; 3; 4|], "1 2 3 4");
    ([|123; 24; 0|], "123 024 00");
    ([|-1; 0|], "-1 0");
    ([|9; 9|], "9 9");
  ] |>
      List.iter (fun (exp, src) ->
        Line_of_numbers.safe_array src |> assert_equal exp)
