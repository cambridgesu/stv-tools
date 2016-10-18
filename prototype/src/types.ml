
type ballot = {
  ballot_weight : int;
  ballot_preferences : int array
}

type tally = {
  total_candidates : int;
  total_seats : int;
  candidate_names : string array;
  ballots : ballot list;
}
