(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Ballot
open Tally

exception Invalid_header (* not two ints *)
exception No_zero_terminator
exception Empty_line
exception Invalid_stop_code
exception Duplicate_candidate_name of string
exception Incomplete
exception Withdrawals_not_supported

let report_err line_no s =
  Printf.sprintf "%s\nError occurred at input line %d\n" s line_no |>
  prerr_endline

let abend line_no s =
  report_err line_no s;
  exit 1

let extract_name line =
  let len = String.length line in
    if len < 2
    then line
    else if line.[0] = '"' && line.[len - 1] = '"'
    then String.sub line 1 (len - 2)
    else line

type ballot_info = (int * int list)

type blt_ctx =
  | No_header
  | Voting of Contest.t * ballot_info list
  | Candidate_names of Contest.t * ballot_info list * string list

let create_context () = No_header

let handle_header line =
  let values = Line_of_numbers.safe_array line in
    (match values with
    | [| candidates ; seats |] ->
       let contest = Contest.create candidates seats in
         Voting (contest, [])
    | _ -> raise Invalid_header)

let handle_vote contest ballots line =
  let values = Line_of_numbers.safe_array line in
  let len = Array.length values in
    (match len with
    | 0 -> raise Empty_line
    | 1 ->
       if values.(0) = 0
       then Candidate_names (contest, ballots, [])
       else raise Invalid_stop_code
    | n when n < 0 ->
       raise Withdrawals_not_supported
    | _ ->
       let final = values.(len - 1) in
         if final <> 0
         then raise No_zero_terminator
         else let weight = values.(0) in
              let prefs = Line_of_numbers.list_without_ends values in
              let ballot_info = (weight, prefs) in
                Voting (contest, ballot_info :: ballots)
    )

let handle_name contest ballot_infos names line =
  let new_name = extract_name line in
    if List.mem new_name names
    then raise (Duplicate_candidate_name new_name)
    else Candidate_names (contest, ballot_infos, new_name :: names)

let handle_line line = function
  | No_header -> handle_header line
  | Voting (hdr, ballots) -> handle_vote hdr ballots line
  | Candidate_names (hdr, ballots, names) -> handle_name hdr ballots names line

let process_blt_file input_stream =
  let initial_ctx = create_context () in
  let rec process_blt_file line_no ctx =
    let line =
      try Some (input_line input_stream)
      with End_of_file -> None
    in
      try let line_no' = line_no + 1 in
            match line with
            | Some l -> handle_line l ctx |> process_blt_file line_no'
            | None -> ctx
      with e ->
        (report_err line_no "Unspecified error";
         raise e) (* abend line_no "Unspecified error" *)
  in
    process_blt_file 1 initial_ctx

let make_tally contest ballot_infos all_names =
  let names = List.tl all_names in
  let candidates =
    List.rev names |>
    List.mapi (fun idx name -> Candidate.create (idx + 1) name)  
  in
    Tally.create contest (List.rev ballot_infos) candidates

let tally_of_context = function
  | Candidate_names (contest, ballot_infos, names) ->
     make_tally contest ballot_infos names
  | _ -> raise Incomplete

let tally_of_blt_stream input_stream =
  process_blt_file input_stream |> tally_of_context
