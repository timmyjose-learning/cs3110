open Lwt_io
open Lwt.Infix

let p =
  read_line stdin >>= fun s1 ->
  let d1 = try int_of_string s1 with _ -> 0 in
  read_line stdin >>= fun s2 ->
  let d2 = try int_of_string s2 with _ -> 0 in
  printf "The sum of %i and %i is %i\n%!" d1 d2 (d1 + d2)

let () = Lwt_main.run p
