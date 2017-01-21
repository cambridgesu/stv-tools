
let run () =
  let tally = Blt.tally_of_blt_stream stdin in
    Tally.dump tally;
    ignore (Stage.initial tally)
        
let () = run ()
