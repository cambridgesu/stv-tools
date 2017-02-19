(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

(*

some of the key invariants are:

  the total number of votes remains constant, over redistributions, exhaustion
  etc

  the sum of the elected, eliminated and continuing candidates is constant

  the number of elected candidates is strictly less than the total number of
  seats to be elected (or the loop exits)

  the number of continuing candidates is strictly more than the number of
  seats remaining unfilled

  the number of votes a non-eliminated candidate has cannot decrease; the
  number of votes an elected candidate has cannot increase

  candidates can only change from continuing to elected or eliminated

  the quantities of initial votes for each candidate are integers

*)

type t = {
  candidacies : (Candidate.t * Status.t) list;
  tally : Tally.t;
}

let continuing_candidates stage =
  (List.filter (fun (cand, status) -> Status.is_continuing status)
     stage.candidacies) |> List.map fst

let ballots_by_continuing_candidates stage =
  let candidates = continuing_candidates stage in
    Tally.by_preferred_candidate candidates stage.tally

(* FIXME: don't use mutable data structure even temporarily,
          and just use fold for the whole thing in the meantime *)
let ballot_totals_by_continuing_candidates stage =
  let assoc_of_hashtbl tbl =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
  in
  let preferences = ballots_by_continuing_candidates stage in
  let totals : (Candidate.t option, int) Hashtbl.t = Hashtbl.create 5 in
    List.iter (fun (weight, candidate) ->
    (try
       let current_total = Hashtbl.find totals candidate in
         Hashtbl.replace totals candidate (current_total + weight)
     with Not_found ->
       Hashtbl.replace totals candidate weight))
      preferences;
    assoc_of_hashtbl totals      

let string_of_candidate_option = function
  | None -> "Exhausted"
  | Some c -> Candidate.name c

let dump_stage stage =
  ballots_by_continuing_candidates stage |>
  List.iter (fun (weight, top_pref) ->
    print_int weight; print_string " ";
    match top_pref with
      | None -> print_endline "None"
      | Some c -> print_endline (Candidate.name c)
  );

  ballot_totals_by_continuing_candidates stage |>
  List.map (fun (c, total) ->
    (string_of_candidate_option c) ^ ": " ^ (string_of_int total)
  )
  |> List.iter print_endline;

  stage

let make_new_stage event old_stage =
  old_stage

let is_valid stage =
  true

let verify_transition old_stage new_stage =
  assert (is_valid old_stage);
  new_stage

let verify_stage stage =
  assert (is_valid stage);
  stage

(* API from here down *)

let initial tally =
  let continuing = Status.create () in
    {
      candidacies = Tally.candidates tally |>
          List.map (fun c -> (c, continuing));
      tally = tally;
    }
  |> dump_stage
  |> verify_stage

let next old_stage event =
  verify_stage old_stage
  |> make_new_stage event
  |> verify_transition old_stage
  |> verify_stage
