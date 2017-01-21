open Ballot

type t = {
  total_candidates : int;
  total_seats : int;
  candidate_names : string list;
  ballots : Ballot.t list;
}

exception Too_many_preferences of Ballot.t

let check_ballot_size max_size ballot =
  if max_size < (Ballot.total_preferences ballot)
  then raise (Too_many_preferences ballot)
  else ()

let check_consistency candidates seats ballots names =
  let check_size = check_ballot_size candidates in
    if candidates <= seats
    then failwith "Enough seats for all candidates; no need for election"
    else if List.length names <> candidates
    then failwith "Number of candidates doesn't match names"
    else ballots |> List.iter check_size

let create contest ballots names =
  let candidates, seats = Contest.get_totals contest in
    check_consistency candidates seats ballots names;
    {
      total_candidates = candidates;
      total_seats = seats;
      candidate_names = List.rev names;
      ballots = ballots;
    }

let dump tally =
  Printf.printf "Seats: %d; Candidates: %d\n\nCandidate names:\n"
    tally.total_seats tally.total_candidates;

  List.iteri (fun i name -> Printf.printf " %d. %s\n" (i + 1) name)
    tally.candidate_names;

  List.iter Ballot.dump tally.ballots
