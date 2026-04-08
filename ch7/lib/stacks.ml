module type LIST_STACK = sig
  exception Empty
  (** [Empty] is raised when an operation cannot be performed on an empty stack
  *)

  val empty : 'a list
  (** [empty] is the empty stack *)

  val is_empty : 'a list -> bool
  (** [is_empty st] returns [true] if the stack [st] is empty *)

  val push : 'a -> 'a list -> 'a list
  (** [push x st] pushes the element [x] onto the stack [st] *)

  val peek : 'a list -> 'a
  (** [peek st] is the top element of [st]. Raises [Empty] if the stack is empty
  *)

  val pop : 'a list -> 'a list
  (** [pop st] is all but the top element of [st]. Raises [Empty] is [st] is
      empty *)
end

module ListStack : LIST_STACK = struct
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x st = x :: st

  exception Empty

  let peek = function [] -> raise Empty | h :: _ -> h
  let pop = function [] -> raise Empty | _ :: t -> t
end

