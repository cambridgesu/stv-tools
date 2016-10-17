
let run () =
  let b = Blt.create_context () in
  let blt_context = ref b in
    try
      while true do
        let line = input_line stdin in
          blt_context := Blt.handle_line line !blt_context
      done;
      None
    with
      End_of_file -> None
        
let _ = run ()
