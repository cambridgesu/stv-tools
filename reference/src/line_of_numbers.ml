
exception Only_ints (* string must be space separated ints *)

let int_array_of_string s =
  Str.split (Str.regexp " ") s |>
  List.map int_of_string |>
  Array.of_list

let safe_array s =
  let values =
    try int_array_of_string s
    with Failure _ -> raise Only_ints
  in
    values

let list_without_ends aa =
  Array.sub aa 1 (Array.length aa - 2) |> Array.to_list
