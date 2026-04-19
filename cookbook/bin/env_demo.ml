open Printf

let () =
  try
    let path = Sys.getenv "PATH" in
    printf "PATH = %s\n" path
  with Not_found -> eprintf "env var PATH does not exist\n"

let () = print_newline ();;

match Sys.getenv_opt "PATH" with
| None -> eprintf "Could not find env var PATH\n"
| Some s -> printf "PATH = %s\n" s