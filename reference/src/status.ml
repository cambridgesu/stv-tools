(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

type t =
  | Continuing
  | Elected
  | Eliminated

let create () = Continuing

let is_continuing = function
  | Continuing -> true
  | _ -> false

let declare_elected = function
  | Continuing -> Elected
  | _ -> assert false

let declare_eliminated = function
  | Continuing -> Eliminated
  | _ -> assert false
