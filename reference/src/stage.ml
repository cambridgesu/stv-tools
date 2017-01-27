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
  previous : t list;
  candidacies : (Candidate.t * Status.t) list
}

let example = {
  previous = [];
  candidacies = [];
}

let initial whatever =
  example

let next old_stage event = old_stage

let consistent stage =
  true
