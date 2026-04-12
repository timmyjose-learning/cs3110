[@@@warning "-32"]

let rec fact_rec = function 0 | 1 -> 1 | n -> n * fact_rec (n - 1)

(* Tying the recursive knot *)

let fact0 = ref (fun x -> x)
let fact = function 0 | 1 -> 1 | n -> n * !fact0 (n - 1);;

(* tying the knot *)
fact0 := fact;;


let () = assert (1 = fact 0)
let () = assert (1 = fact 1)
let () = assert (2 = fact 2)
let () = assert (6 = fact 3)
let () = assert (24 = fact 4)
let () = assert (120 = fact 5)
