
open Ballot
open Tally

type blt_ctx =
  | No_header
  | Voting of int * int * Ballot.t list
  | Candidate_names of int * int * Ballot.t list * string array

exception Only_ints (* string must be space separated ints *)
exception Invalid_header (* not two ints *)
exception No_zero_terminator
exception Empty_line
exception Invalid_stop_code
exception Non_positive_pref of int
exception Non_consecutive_prefs
exception Duplicate_candidate_name of string
exception Incomplete

let int_array_of_string s =
  List.map int_of_string (Str.split (Str.regexp " ") s) |> Array.of_list

let safe_int_array_of_string s =
  let values =
    try int_array_of_string s
    with Failure _ -> raise Only_ints
  in
    values

let check_preferences prefs =
  prefs |> Array.iter (
    fun pref -> if pref < 1 then raise (Non_positive_pref pref) else ()
  );
  let sorted = Array.to_list prefs |> List.sort compare |> Array.of_list in
  let len = Array.length prefs in
  let last_pref = sorted.(len - 1) in
    if last_pref <> len
    then raise Non_consecutive_prefs
    else ()

let extract_name line =
     let len = String.length line in
       if len < 2
       then line
       else if line.[0] = '"' && line.[len - 1] = '"'
            then String.sub line 1 (len - 2)
            else line

let create_context () = No_header

let handle_line line line_no = function
  | No_header ->
     let values = safe_int_array_of_string line in
       (match values with
       | [| hd ; tl |] -> Voting (hd, tl, [])
       | _ -> raise Invalid_header)

  | Voting (candidates, seats, ballots) ->
     let values = safe_int_array_of_string line in
     let len = Array.length values in
       (match len with
       | 0 -> raise Empty_line
       | 1 ->
          if values.(0) = 0
          then Candidate_names (candidates, seats, ballots, [||])
          else raise Invalid_stop_code
       | _ ->
          let final = values.(len - 1) in
            if final <> 0
            then raise No_zero_terminator
            else let ballot = {
                   ballot_weight = values.(0);
                   ballot_preferences = Array.sub values 1 (len - 2)
                 } in
                   check_preferences ballot.ballot_preferences;
                   Voting (candidates, seats, [ballot])
       )

  | Candidate_names (candidates, seats, ballots, names) ->
     let new_name = extract_name line in
       if Utils.array_mem new_name names
       then raise (Duplicate_candidate_name new_name)
       else Candidate_names (candidates, seats, ballots,
                             Array.append names [| new_name |])

let abend line_no s =
  print_endline s;
  Printf.printf "Error occurred at input line %d\n" line_no;
  exit 1

let process_blt_file input_stream ctx =
  let rec process_blt_file line_no ctx =
    let line =
      try Some (input_line input_stream)
      with End_of_file -> None
    in
      try
        let line_no' = line_no + 1 in
          match line with
          | Some l -> handle_line l line_no' ctx |> process_blt_file line_no'
          | None -> ctx
      with
        Non_consecutive_prefs -> abend line_no "Non-consecutive preferences"
  in
    process_blt_file 1 ctx

let tally_of_context ctx = 
  match ctx with
  | Candidate_names (candidates, seats, ballots, names) ->
     Tally.create candidates seats ballots names
  | _ -> raise Incomplete

let tally_of_blt_stream input_stream =
  create_context () |> process_blt_file input_stream |> tally_of_context

