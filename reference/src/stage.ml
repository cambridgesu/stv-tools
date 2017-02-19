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

let ballots_by_continuing_candidates stage =
  let continuing_candidates =
    (List.filter (fun (cand, status) -> Status.is_continuing status)
      stage.candidacies) |> List.map fst
  in
    Tally.by_preferred_candidate continuing_candidates stage.tally

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
  |> verify_stage

let next old_stage event =
  verify_stage old_stage
  |> make_new_stage event
  |> verify_transition old_stage
  |> verify_stage
