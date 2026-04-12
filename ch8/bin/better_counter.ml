(* closure *)
let next_val =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let better_counter_demo () =
  for _ = 1 to 10 do
    Printf.printf "%i\n" (next_val ())
  done

let () = better_counter_demo ()