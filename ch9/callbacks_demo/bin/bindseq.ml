open Lwt_io
open Lwt.Infix

let p =
  read_line stdin >>= fun s1 ->
  read_line stdin >>= fun s2 -> printf "%s\n" (s1 ^ ", " ^ s2)

let () = Lwt_main.run p