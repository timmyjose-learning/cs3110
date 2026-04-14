let _ =
  Domain.spawn (fun _ -> print_endline "I ran in parallel!");

  for _ = 1 to 10 do
    Domain.spawn (fun _ -> print_endline "Hello")
  done