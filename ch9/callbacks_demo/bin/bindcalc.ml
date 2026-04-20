open Lwt_io
open Lwt.Infix

let p1 () =
  Lwt.bind (read_line stdin) (fun s1 ->
      let d1 = try int_of_string s1 with _ -> 0 in
      Lwt.bind (read_line stdin) (fun s2 ->
          let d2 = try int_of_string s2 with _ -> 0 in
          let sum = d1 + d2 in
          let diff = d1 - d2 in
          let prod = d1 * d2 in
          let quot = if d2 = 0 then 0 else d1 / d2 in
          printf "Sum = %i, Diff = %i, Prod = %i, Quot = %i\n%!" sum diff prod
            quot))

let p2 () =
  read_line stdin >>= fun s1 ->
  let d1 = try int_of_string s1 with _ -> 0 in
  read_line stdin >>= fun s2 ->
  let d2 = try int_of_string s2 with _ -> 0 in
  let sum = d1 + d2 in
  let diff = d1 - d2 in
  let prod = d1 * d2 in
  let quot = if d2 = 0 then 0 else d1 / d2 in
  printf "Sum = %i, diff = %i, prod = %i, quot = %i\n%!" sum diff prod quot

let main () = p1 () >>= fun _ -> p2 ()
let () = Lwt_main.run (main ())