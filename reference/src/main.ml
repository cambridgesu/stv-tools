(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

let run () =
  let tally = Blt.tally_of_blt_stream stdin in
    Tally.dump tally;
    let st = Stage.initial tally in
    let ev = Stage.resultant_event st in
      Event.string_of ev |> print_endline

let () = run ()
