[@@@warning "-32-34-37"]

(* Data and Types *)
let rec sum = function [] -> 0 | h :: t -> h + sum t

let sum_tco lst =
  let rec aux acc = function [] -> acc | h :: t -> aux (acc + h) t in
  aux 0 lst

let rec length = function [] -> 0 | _ :: t -> 1 + length t

let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | h :: t -> h :: append t lst2

let empty = function [] -> true | _ -> false
let inc_first = function [] -> [] | h :: t -> (h + 1) :: t

(** [from i j l] is the list containing the integers from [i] to [j], inclusive,
    followed by the list [l]. * Example: [from 1 3 [0]] = [1; 2; 3; 0] *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

let () = assert (from 1 3 [ 0 ] = [ 1; 2; 3; 0 ])

(** [i -- j] is the list containing the numbers from [i] to [j], inclusive *)
let ( -- ) i j = from i j []

let () = assert (1 -- 10 = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ])

(* Variants *)

type day =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

let d = Tuesday

let int_of_day = function
  | Sunday -> 1
  | Monday -> 2
  | Tuesday -> 3
  | Wednesday -> 4
  | Thursday -> 5
  | Friday -> 6
  | Saturday -> 7

type ptype = TNormal | TFire | TWater
type peff = ENormal | ENotVery | ESuper

let fst (f, _) = f
let snd (_, s) = s
let third t = match t with _, _, z -> z

let third t =
  let _, _, z = t in
  z

let third (_, _, z) = z

(* Type Synonyms *)

type point = float * float
type vector = float list
type matrix = float list list

let get_x = fun (x, _) -> x
let p1 : point = (1., 2.)
let p2 : float * float = (1., 3.)
let a = get_x p1
let b = get_x p2

(* This still works with float * float even if explicitly typed - so, type safety is reduced using type synonyms *)
let get_x_typed ((x, _) : point) = x
