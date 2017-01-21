
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
open Types

module Stage : sig
  type t
  val initial : tally -> t
  val next : t -> Event.t -> t
end =
struct
  type t = {
    x : int;
    previous : t list;
    elected_candidates : int;
  }

  let example = {
    x = 3;
    previous = [];
    elected_candidates = 0;
  }

  let initial whatever =
    example

  let next old_stage event = old_stage

  let consistent stage =
    true

end
