[@@@warning "-32"]

(* Higher-Order Programming *)

let double x = 2 * x
let square x = x * x
let quad x = double (double x)
let fourth x = square (square x)
let twice f x = f (f x)
let quad x = twice double x
let fourth x = twice square x
let apply f x = f x
let ( |>> ) x f = f x
let compose f g x = f (g x)
let square_then_double = compose double square
let double_then_square = compose square double

(* apply two functions to the same argument, and return the results as a tuple *)
let both f g x = (f x, g x)
let sd = both square double

(* choose which of two functions to apply, based on a predicate *)
let cond p f g x = if p then f x else g x

(* map *)

(** [add1 lst] adds 1 to each element of the list *)
let rec add1 = function [] -> [] | h :: t -> (h + 1) :: add1 t

(** [concat_bang lst] apends "!" to each element of the list *)
let rec concat_bang = function [] -> [] | h :: t -> (h ^ "!") :: concat_bang t

(* map *)

let rec map f = function [] -> [] | h :: t -> f h :: map f t
let add1 = map (fun x -> x + 1)
let concat_bang = map (fun x -> x ^ "!'")

let p x =
  print_int x;
  print_newline ();
  x + 1

let lst = map p [ 1; 2 ]

let rec map f = function
  | [] -> []
  | h :: t ->
      let h' = f h in
      h' :: map f t

let q x =
  print_int x;
  print_newline ();
  x + 1

let lst' = map q [ 1; 2 ]

let rev_map f =
  let rec aux acc f = function
    | [] -> acc
    | h :: t ->
        let h' = f h in
        aux (h' :: acc) f t
  in
  aux [] f

(* filter *)

let even n = n mod 2 = 0

let rec evens = function
  | [] -> []
  | h :: t -> if even h then h :: evens t else evens t

let one_to_hundred = List.init 100 (fun x -> x + 1)
let odd n = n mod 2 <> 0

let rec odds = function
  | [] -> []
  | h :: t -> if odd h then h :: odds t else odds t

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let evens' = filter even
let odds' = filter odd

let filter_tco p =
  let rec aux acc p = function
    | [] -> List.rev acc
    | h :: t -> if p h then aux (h :: acc) p t else aux acc p t
  in
  aux [] p

(* fold *)

let rec sum = function [] -> 0 | h :: t -> h + sum t
let rec concat = function [] -> "" | h :: t -> h ^ concat t

let rec combine op init = function
  | [] -> init
  | h :: t -> op h (combine op init t)

let sum' = combine ( + ) 0
let concat' = combine ( ^ ) ""

let rec fold_right f lst acc =
  match lst with [] -> acc | h :: t -> f h (fold_right f t acc)

let sum'' lst = fold_right ( + ) lst 0
let concat'' lst = fold_right ( ^ ) lst ""

let rec combine_tco f acc = function
  | [] -> acc
  | h :: t -> combine_tco f (f acc h) t

let rec fold_left f acc = function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let sum''' = fold_left ( + ) 0
let concat''' = fold_left ( ^ ) ""