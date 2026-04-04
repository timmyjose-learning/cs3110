(* Natural Numbers *)

type nat = Zero | Succ of nat

let zero = Zero
let one = Succ Zero
let two = Succ one
let three = Succ two
let iszero = function Zero -> true | Succ _ -> false

let pred = function
  | Zero -> failwith "Zero doesn't have a predecessor"
  | Succ n -> n

let rec add n m = match n with Zero -> m | Succ n' -> Succ (add n' m)
let rec nat_to_int = function Zero -> 0 | Succ n -> 1 + nat_to_int n

let rec nat_of_int = function
  | 0 -> Zero
  | n when n > 0 -> Succ (nat_of_int (n - 1))
  | _ -> failwith "negative numbers"