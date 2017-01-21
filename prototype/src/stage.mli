
type t

val initial : Tally.t -> t

val next : t -> Event.t -> t
