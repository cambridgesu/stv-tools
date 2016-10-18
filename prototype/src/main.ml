
let run () =
  let tally = Blt.tally_of_blt_stream stdin in
    ignore tally;
    print_endline "DONE"
        
let _ = run ()
