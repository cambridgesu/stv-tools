
let run () =
  let process_blt_file input_stream ctx =
    let rec process_blt_file ctx =
      let line =
        try Some (input_line input_stream)
        with End_of_file -> None
      in
        match line with
        | Some l -> let new_ctx = Blt.handle_line l ctx in
                      process_blt_file new_ctx
        | None -> ctx
    in
      process_blt_file ctx
  in
      Blt.create_context () |> process_blt_file stdin |> Blt.check_consistency;
      print_endline "DONE"
        
let _ = run ()
