
exception Only_ints (* string must be space separated ints *)
exception Invalid_line_of_numbers

let int_array_of_string s =
  Str.split (Str.regexp " ") s |>
  List.map int_of_string |>
  Array.of_list

(* FIXME: check for double space *)
let safe_array s =
  if (String.length s > 0) && (s.[0] <> ' ') && (s.[String.length s - 1] <> ' ')
  then try int_array_of_string s
       with Failure _ -> raise Only_ints
  else raise Invalid_line_of_numbers

let list_without_ends aa =
  Array.sub aa 1 (Array.length aa - 2) |> Array.to_list
