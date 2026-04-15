let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let rec fib_par n =
  if n < 20 then fib n
  else
    let d1 = Domain.spawn (fun _ -> fib_par (n - 1)) in
    let d2 = Domain.spawn (fun _ -> fib_par (n - 2)) in
    Domain.join d1 + Domain.join d2

let main () =
  let n = try int_of_string Sys.argv.(1) with _ -> 1 in
  let r = fib_par n in
  Printf.printf "fib(%d) = %d\n" n r

let () = main ()
