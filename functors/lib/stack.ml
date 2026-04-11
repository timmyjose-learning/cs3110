module type Stack = sig
  type 'a t

  exception Empty

  val empty : 'a t
  val is_empty : 'a t -> bool
  val peek : 'a t -> 'a
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
end

module ListStack : Stack = struct
  type 'a t = 'a list

  exception Empty

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let peek = function [] -> raise Empty | h :: _ -> h
  let push = List.cons
  let pop = function [] -> raise Empty | _ :: t -> t
end

module VariantStack : Stack = struct
  type 'a t = E | S of 'a * 'a t

  exception Empty

  let empty = E
  let is_empty = function E -> true | S _ -> false
  let push x s = S (x, s)
  let peek = function E -> raise Empty | S (h, _) -> h
  let pop = function E -> raise Empty | S (_, t) -> t
end
