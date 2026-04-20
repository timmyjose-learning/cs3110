let print_the_string s = Lwt_io.printf "You entered: %s\n" s

let main () =
  let p = Lwt_io.read_line Lwt_io.stdin in
  Lwt.bind p print_the_string

let () = Lwt_main.run (main ())