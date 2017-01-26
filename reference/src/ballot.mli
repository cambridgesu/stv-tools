(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type t

exception Duplicate_prefs

val create : Contest.t -> Candidate.t list -> int -> int list -> t
val total_preferences : t -> int
val dump : t -> unit
val dump_named : string list -> t -> unit
