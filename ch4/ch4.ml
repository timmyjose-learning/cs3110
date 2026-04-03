(* disable unused warnings *)
[@@@warning "-32"]

let rec fact n = if n <= 0 then 1 else n * fact (n - 1)
let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)

(* mutual recursion *)

let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1)

let inc x = x + 1
let square x = x * x
let sqr_then_inc x = square x |> inc
let inc_then_sqr x = inc x |> square

(* labelled and optional arguments *)
let myadd ~x ~y = x + y
let mysub ~string ~startpos ~len = String.sub string startpos len
let mysub' ~string ~startpos ~len = String.sub string startpos len
let mysub'' ~string ?(startpos = 0) len = String.sub string startpos len

(* operator sections *)

let add10 = ( + ) 10
let ( ^^ ) = max

(* Tail Recursion *)

let rec count n = if n = 0 then 0 else 1 + count (n - 1)

let count_tco n =
  let rec aux acc n = match n with 0 -> acc | _ -> aux (acc + 1) (n - 1) in
  aux 0 n

let fact_tco n =
  let rec aux acc n = match n with 0 -> acc | _ -> aux (n * acc) (n - 1) in
  aux 1 n

(*
 * $ opam install zarith
 * $ #require "zarith.top";; in utop
 *)
let zfact n =
  let rec aux acc n =
    if Z.equal n Z.zero then acc else aux (Z.mul acc n) (Z.pred n)
  in
  aux Z.one (Z.of_int n)

(** [sum lst] is the sum of the elements of [lst]. * The sum of an empty list is
    0. *)
let sum lst =
  let rec aux acc lst = match lst with [] -> 0 | h :: t -> aux (acc + h) t in
  aux 0 lst

let print_stat name num =
  print_string name;
  print_string ": ";
  print_float num;
  print_newline ()

let print_stat_better name num = Printf.printf "%s: %F%!\n" name num
let string_of_stat name num = Printf.sprintf "%s: %F%!\n" name num