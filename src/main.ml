(*
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
*)

let run () =
  let rec process_blt_file ctx =
    let line =
      try Some (input_line stdin)
      with End_of_file -> None
    in
      match line with
        | Some l -> let new_ctx = Blt.handle_line l ctx in
                      process_blt_file new_ctx
        | None -> ctx
  in
    let tally = Blt.create_context () |> process_blt_file in
      Blt.check_consistency tally;
      print_endline "DONE"
        
let _ = run ()
