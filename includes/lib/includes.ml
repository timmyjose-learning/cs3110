module type Set = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val elements : 'a t -> 'a list
end

module ListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end

module type SetExtendsd = sig
  include Set

  val of_list : 'a list -> 'a t
end

module ListSetExtended : SetExtendsd = struct
  include ListSet

  let of_list lst = List.fold_right add lst empty
end

(* include vs open *)

module M = struct
  let x = 0
end

module N = struct
  include M

  let y = x + 1
end

module O = struct
  open M

  let y = x + 1
end

(* including code in multiple modules *)

module UniqListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let mem = List.mem
  let add x s = if mem x s then s else x :: s
  let elements = Fun.id
end
