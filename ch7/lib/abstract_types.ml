module type Stack = sig
  type 'a t
  (** the abstract type of the implementation of the t *)

  exception Empty

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
  val size : 'a t -> int
end

module ListStack : Stack = struct
  type 'a t = 'a list
  (** concrete implementation type *)

  exception Empty

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x st = x :: st
  let peek = function [] -> raise Empty | h :: _ -> h
  let pop = function [] -> raise Empty | _ :: t -> t
  let size = List.length
end

module _ : Stack = ListStack

module ListStackCachedSize : Stack = struct
  type 'a t = 'a list * int
  (** concrete implementation type *)

  exception Empty

  let empty = ([], 0)
  let is_empty = function [], _ -> true | _ -> false
  let push x (st, sz) = (x :: st, sz + 1)
  let peek = function [], _ -> raise Empty | h :: _, _ -> h
  let pop = function [], _ -> raise Empty | _ :: t, sz -> (t, sz - 1)
  let size (_, sz) = sz
end

(* module CheckListStackCachedSize : Stack = ListStackCachedSize *)
module _ : Stack = ListStackCachedSize

module CustomStack : Stack = struct
  type 'a entry = { top : 'a; rest : 'a t; size : int }
  and 'a t = S of 'a entry option

  exception Empty

  let empty = S None
  let is_empty = function S None -> true | S (Some _) -> false
  let size = function S None -> 0 | S (Some { size; _ }) -> size
  let push x st = S (Some { top = x; rest = st; size = size st + 1 })
  let peek = function S None -> raise Empty | S (Some { top; _ }) -> top
  let pop = function S None -> raise Empty | S (Some { rest; _ }) -> rest
end

module _ : Stack = CustomStack
