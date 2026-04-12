(* Mutabilty *)

(* Aliasing *)

let aliasing_demo () =
  let x = ref 42 in
  let y = ref 42 in
  let z = x in
  x := 43;
  let w = !y + !z in
  Printf.printf "%i\n" w

let () = aliasing_demo ()