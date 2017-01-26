(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type t = {
  cand_position : int;
  cand_name : string
}

let eq a b = a.cand_position = b.cand_position

let create pos name = {
  cand_position = pos;
  cand_name = name;
}

let name c = c.cand_name

let position c = c.cand_position
