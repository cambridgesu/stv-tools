
let run () =
  let b = Blt.create_context () in
  let blt_context = ref b in
    (try
      while true do
        let line = input_line stdin in
          blt_context := Blt.handle_line line !blt_context
      done
    with
      End_of_file -> ());
    Blt.check_consistency !blt_context
        
let _ = run ()
