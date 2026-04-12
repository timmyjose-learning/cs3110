let counter = ref 0

let next_val =
 fun () ->
  counter := !counter + 1;
  !counter

let counter_demo () =
  for _ = 1 to 10 do
    Printf.printf "%i\n" (next_val ())
  done

let () = counter_demo ()
