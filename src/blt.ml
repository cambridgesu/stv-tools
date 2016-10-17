
type ballot = {
  ballot_weight : int;
  ballot_preferences : int array
}

type blt_ctx =
  | No_header
  | Header of int * int
  | Voting of int * int * ballot list
  | All_votes of int * int * ballot list
  | Candidate_names of int * int * ballot list * string array

exception Only_ints (* string must be space separated ints *)
exception Invalid_header (* not two ints *)
exception No_zero_terminator
exception Empty_or_malformed_ballot

let int_array_of_string s =
  List.map int_of_string (Str.split (Str.regexp " ") s) |> Array.of_list

let safe_int_array_of_string s =
  let values =
    try int_array_of_string s
    with Failure _ -> raise Only_ints
  in
    values

let create_context () = No_header

let handle_line line = function
  | No_header ->
     let values = safe_int_array_of_string line in
       (match values with
       | [| hd ; tl |] -> Header (hd, tl)
       | _ -> raise Invalid_header)

  | Header (hd, tl) ->
     let values = safe_int_array_of_string line in
     let len = Array.length values in
       if len < 2
       then raise Empty_or_malformed_ballot
       else let final = values.(len - 2) in
              if final <> 0
              then raise No_zero_terminator
              else let ballot = {
                     ballot_weight = values.(0);
                     ballot_preferences = Array.sub values 1 (len - 1)
                   } in
                     Voting (hd, tl, [ballot])
  | Voting (hd, tl, ballots) -> print_endline "No handler";
    Voting (hd, tl, ballots)
  | _ -> assert false
