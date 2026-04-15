let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

module T = Domainslib.Task

let rec fib_par pool n =
  if n < 20 then fib n
  else
    let d1 = T.async pool (fun _ -> fib_par pool (n - 1)) in
    let d2 = T.async pool (fun _ -> fib_par pool (n - 2)) in
    T.await pool d1 + T.await pool d2

let main () =
  let n = try int_of_string Sys.argv.(1) with _ -> 1 in
  let num_domains = try int_of_string Sys.argv.(2) with _ -> 2 in
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let res = T.run pool (fun _ -> fib_par pool n) in
  Printf.printf "fib(%d) = %d\n" n res;
  T.teardown_pool pool

let () = main ()
