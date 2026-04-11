module type Set = sig
  type 'a t

  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val elements : 'a t -> 'a list
end

module SetWithOfList (S : Set) = struct
  include S

  let of_list lst = List.fold_right S.add lst S.empty
end

module ListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements = Fun.id
end

module UniqListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add x s = if mem x s then s else x :: s
  let elements = Fun.id
end

module SetL = SetWithOfList (ListSet)
module UniqSetL = SetWithOfList (UniqListSet)
