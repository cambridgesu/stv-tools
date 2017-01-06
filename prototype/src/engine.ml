
(*

some of the key invariants are:

  the total number of votes remains constant, over redistributions, exhaustion
  etc

  the sum of the elected, eliminated and continuing candidates is constant

  the number of elected candidates is strictly less than the total number of
  seats to be elected (or the loop exits)

  the number of continuing candidates is strictly more than the number of
  seats remaining unfilled

*)
open Types

type event =
  | Elected
  | Excluded
  | Distribute_surplus
  | Distribute_excluded

module Stage : sig
  type t
  val initial : tally -> t
  val next : t -> event -> t
end =
struct
  type t = {
    x : int;
    previous : t list;
  }

  let example = {
    x = 3;
    previous = [];
  }

  let initial whatever =
    example

  let next old_stage event = old_stage

  let consistent stage =
    true

end
