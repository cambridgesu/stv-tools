(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type t

val create : Contest.t -> (int * int list) list -> Candidate.t list -> t

val candidates : t -> Candidate.t list

val dump : t -> unit

val by_preferred_candidate : Candidate.t list -> t ->
  (int * Candidate.t option) list
