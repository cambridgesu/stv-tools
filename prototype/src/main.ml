
let run () =
  let tally = Blt.tally_of_blt_stream stdin in
    print_endline "DONE";
    Engine.Stage.initial tally
        
let _ = run ()
