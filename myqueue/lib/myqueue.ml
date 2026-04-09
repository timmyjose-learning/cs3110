module type Queue = sig
  type 'a t
  (** An ['a t] is a queue whose elements have type ['a] *)

  exception Empty
  (** Raised if [front] or [dequeue] is invoked on an empty queue *)

  val empty : 'a t
  (** [empty] is the empty queue *)

  val is_empty : 'a t -> bool
  (** [is_empty q] checks whether the queue [q] is empty *)

  val enqueue : 'a -> 'a t -> 'a t
  (** [enqueue x q] is the queue [q] with [x] added to the end *)

  val front : 'a t -> 'a
  (** [front q] is the element at the front of the queue. Raises [Empty] is [q]
      is empty *)

  val dequeue : 'a t -> 'a t
  (** [dequeue q] is the queue containing all the elements of [q] except the
      front of [q]. Raise [Emppty] if [q] is empty *)

  val size : 'a t -> int
  (** [size q] returns the size of [q] *)

  val to_list : 'a t -> 'a list
  (** [to_list q] is a list containing the elements of [q] in order from front
      to back *)
end

module ListQueue : Queue = struct
  type 'a t = 'a list

  exception Empty

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let enqueue x q = q @ [ x ]
  let front = function [] -> raise Empty | h :: _ -> h
  let dequeue = function [] -> raise Empty | _ :: t -> t
  let size = List.length
  let to_list = Fun.id
end

module BatchedQueue : Queue = struct
  type 'a t = { o : 'a list; i : 'a list }

  exception Empty

  let empty = { o = []; i = [] }
  let is_empty = function { o = []; _ } -> true | _ -> false

  let enqueue x = function
    | { o = []; _ } -> { o = [ x ]; i = [] }
    | { o; i } -> { o; i = x :: i }

  let front = function { o = []; _ } -> raise Empty | { o = h :: _ ; _} -> h

  let dequeue = function
    | { o = []; _ } -> raise Empty
    | { o = [ _ ]; i } -> { o = List.rev i; i = [] }
    | { o = _ :: t; i } -> { o = t; i }

  let size { o; i } = List.(length o + length i)
  let to_list { o; i } = o @ List.rev i
end