[@@@warning "-32"]
(* Algebraic Data Types *)

type point = float * float
type shape = Point of point | Circle of point * float | Rect of point * point

let area = function
  | Point _ -> 0.
  | Circle (_, r) -> Float.pi *. r *. r
  | Rect ((x1, y1), (x2, y2)) ->
      let w = x2 -. x1 in
      let h = y2 -. y1 in
      w *. h

let centre = function
  | Point p -> p
  | Circle (p, _) -> p
  | Rect ((x1, y1), (x2, y2)) -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)

type string_or_int = String of string | Int of int
type string_or_int_list = string_or_int list

let rec sum : string_or_int list -> int = function
  | [] -> 0
  | String s :: t -> int_of_string s + sum t
  | Int d :: t -> d + sum t

let l1 = [ String "1"; Int 2; String "3"; Int 4; String "5"; Int 6 ]

type either_int = Left of int | Right of int

let double_right = function Left d -> d | Right d -> d * 2
let () = assert (1 = double_right (Left 1))
let () = assert (2 = double_right (Right 1))

(* catch-all cases *)

type color = Blue | Red | Green | Orange

(** This is buggy since extending [color] will not give a warning or error about
    the catch-all *)
let string_of_color = function Blue -> "blue" | _ -> "red"

(** This is better since now not handling extensions to [color] will be a
    compile-time warning *)
let string_of_color = function
  | Blue -> "blue"
  | Red -> "red"
  | Green -> "green"
  | Orange -> "orange"

(* Recursive variants *)

type mylist = Nil | Cons of int * mylist

let lst1 = Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Nil)))))
let lst2 = Cons (6, Cons (7, Nil))
let rec sum = function Nil -> 0 | Cons (h, t) -> h + sum t
let () = assert (15 = sum lst1)
let () = assert (13 = sum lst2)
let rec length = function Nil -> 0 | Cons (_, t) -> 1 + length t
let () = assert (5 = length lst1)
let () = assert (2 = length lst2)
let empty = function Nil -> true | Cons _ -> false
let () = assert (empty Nil)
let () = assert (not (empty lst1))
let () = assert (not (empty lst2))

(* mutually recursive *)
type node = { value : int; next : neolist }
and neolist = Nil | Node of node

type t = U of u
and u = T of t

(*type node = { value : int; next : node }*)

type ('a, 'b) pair = { first : 'a; second : 'b }

let p = { first = 100; second = "Hello" }

(* Polymorphic Variants *)

type fin_or_inf = Finite of int | Infinity

let f = function 0 -> Infinity | 1 -> Finite 1 | n -> Finite (-n)
let f = function 0 -> `Infinity | 1 -> `Finite 1 | n -> `Finite (-n)


