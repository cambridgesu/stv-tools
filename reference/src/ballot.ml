(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type t = {
  ballot_weight : int;
  ballot_preferences : Candidate.t list
}

exception Duplicate_prefs
exception Non_positive_pref of int
exception No_such_candidate of int * int

let rec no_duplicates = function
  | [] -> true
  | hd :: tl ->
     if (List.mem hd tl)
     then false
     else no_duplicates tl

let check_preferences contest prefs =
  let max_candidate, _ = Contest.get_totals contest in
    prefs |> List.iter (
      fun pref -> 
        if pref > max_candidate
        then raise (No_such_candidate (pref, max_candidate))
        else ()
    );

    prefs |> List.iter (
      fun pref -> if pref < 1 then raise (Non_positive_pref pref) else ()
    );

    if no_duplicates prefs
    then ()
    else raise Duplicate_prefs

let create contest candidates weight prefs =
  let lookup_candidate n = List.nth candidates (n - 1) in
    check_preferences contest prefs;
    {
      ballot_weight = weight;
      ballot_preferences = List.map lookup_candidate prefs
    }

let total_preferences b =
  List.length b.ballot_preferences

let dump b =
  Printf.printf "\nBallot weight: %d\n\n" b.ballot_weight;
  List.iteri (fun i pref ->
    Printf.printf "  %d: Option %d\n" (i + 1) (Candidate.position pref)
  ) b.ballot_preferences

let dump_named names b =
  Printf.printf "\nBallot weight: %d\n\n" b.ballot_weight;
  List.iteri (fun i pref ->
    Printf.printf "  %d: %s\n" (i + 1) (Candidate.name pref)
  ) b.ballot_preferences

let preferred_candidate continuing_candidates b =
  let matches pref = List.mem pref continuing_candidates in
  let matching_preferences = List.filter matches b.ballot_preferences in
  let best_preference = match matching_preferences with
      | [] -> None
      | top_pref :: _ -> Some top_pref
  in
    (b.ballot_weight, best_preference)
