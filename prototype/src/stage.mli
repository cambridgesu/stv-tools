
open Types

type t

val initial : tally -> t

val next : t -> Event.t -> t
