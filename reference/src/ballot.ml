
type t = {
  ballot_weight : int;
  ballot_preferences : int list
}

exception Duplicate_prefs
exception Non_positive_pref of int
exception No_such_candidate of int

let rec no_duplicates = function
  | [] -> true
  | hd :: tl ->
     if (List.mem hd tl)
     then false
     else no_duplicates tl

let check_preferences contest prefs =
  let max_candidate, _ = Contest.get_totals contest in
    prefs |> List.iter (
      fun pref -> if pref > max_candidate then raise (No_such_candidate pref)
          else ()
    );

    prefs |> List.iter (
      fun pref -> if pref < 1 then raise (Non_positive_pref pref) else ()
    );

    if no_duplicates prefs
    then ()
    else raise Duplicate_prefs

let create contest weight prefs =
  check_preferences contest prefs;
  {
    ballot_weight = weight;
    ballot_preferences = prefs
  }

let total_preferences b =
  List.length b.ballot_preferences

let dump b =
  Printf.printf "\nBallot weight: %d\n\n" b.ballot_weight;
  List.iteri (fun i pref ->
    Printf.printf "  %d: Option %d\n" (i + 1) pref
  ) b.ballot_preferences

let dump_named names b =
  Printf.printf "\nBallot weight: %d\n\n" b.ballot_weight;
  List.iteri (fun i pref ->
    Printf.printf "  %d: %s\n" (i + 1) (List.nth names (pref - 1))
  ) b.ballot_preferences
  
