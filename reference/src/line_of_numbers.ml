
exception Only_ints (* string must be space separated ints *)

let int_array_of_string s =
  Str.split (Str.regexp " ") s |>
  List.map int_of_string |>
  Array.of_list

let safe_array s =
  assert (String.length s > 0);
  assert (s.[0] <> ' ');
  assert (s.[String.length s - 1] <> ' ');

  try int_array_of_string s
  with Failure _ -> raise Only_ints

let list_without_ends aa =
  Array.sub aa 1 (Array.length aa - 2) |> Array.to_list
