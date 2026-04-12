let rec fib_rec = function
  | (0 | 1) as n -> n
  | m -> fib_rec (m - 1) + fib_rec (m - 2)

let () = assert (0 = fib_rec 0)
let () = assert (1 = fib_rec 1)
let () = assert (1 = fib_rec 2)
let () = assert (2 = fib_rec 3)
let () = assert (3 = fib_rec 4)
let () = assert (5 = fib_rec 5)
let () = assert (8 = fib_rec 6)
let () = assert (13 = fib_rec 7)
let () = assert (21 = fib_rec 8)
let () = assert (34 = fib_rec 9)
let () = assert (55 = fib_rec 10)
let fib0 = ref (fun x -> x)
let fib = function (0 | 1) as n -> n | m -> !fib0 (m - 1) + !fib0 (m - 2);;

(* tie the recursive knot *)
fib0 := fib;;

let () = assert (0 = fib 0)
let () = assert (1 = fib 1)
let () = assert (1 = fib 2)
let () = assert (2 = fib 3)
let () = assert (3 = fib 4)
let () = assert (5 = fib 5)
let () = assert (8 = fib 6)
let () = assert (13 = fib 7)
let () = assert (21 = fib 8)
let () = assert (34 = fib 9)
let () = assert (55 = fib 10)
