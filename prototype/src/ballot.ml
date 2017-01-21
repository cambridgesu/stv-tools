
type t = {
  ballot_weight : int;
  ballot_preferences : int array
}

exception Non_consecutive_prefs
exception Non_positive_pref of int

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

let create weight prefs =
  check_preferences prefs;
  {
    ballot_weight = weight;
    ballot_preferences = prefs
  }

let total_preferences b =
  Array.length b.ballot_preferences
