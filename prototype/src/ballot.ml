
type t = {
  ballot_weight : int;
  ballot_preferences : int array
}

exception Duplicate_prefs
exception Non_positive_pref of int

let rec no_duplicates = function
  | [] -> true
  | hd :: tl ->
     if (List.mem hd tl)
     then false
     else no_duplicates tl

let check_preferences prefs =
  prefs |> Array.iter (
    fun pref -> if pref < 1 then raise (Non_positive_pref pref) else ()
  );
  if no_duplicates (Array.to_list prefs)
  then ()
  else raise Duplicate_prefs

let create weight prefs =
  check_preferences prefs;
  {
    ballot_weight = weight;
    ballot_preferences = prefs
  }

let total_preferences b =
  Array.length b.ballot_preferences
