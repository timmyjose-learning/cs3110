let write_to_file filename contents =
  try
    Out_channel.with_open_text filename (fun out ->
        Out_channel.output_string out contents)
  with Sys_error err -> failwith ("Failed to write to file: " ^ err)

let read_from_file filename =
  try
    let contents = In_channel.with_open_text filename In_channel.input_all in
    Format.printf "%s%!" contents
  with Sys_error err -> failwith ("Failed to read from file: " ^ err)

let main () =
  let contents = "Hello, world!\nWe meet again!\n" in
  let () = write_to_file "test.txt" contents in
  read_from_file "test.txt"
;;

main ()
