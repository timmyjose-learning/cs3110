module type X = sig
  val x : int
end

(* A functor is simply a mapping from modules to modules, or rather parameterisation over modules *)
module IncX (M : X) = struct
  let x = M.x + 1
end

module A = struct
  let x = 0
end

module B = IncX (A)

module IncY (M : sig
  val y : int
end) =
struct
  let y = M.y + 2
end

module C = IncY (struct
  let y = 100
end)

module D = IncY (C)

module AddX (M : X) = struct
  let add y = M.x + y
end

module type Add = sig
  val add : int -> int
end

module CheckAddX (_ : X) = AddX

module Add10 = AddX (struct
  let x = 10
end)

module SubX =
functor
  (M : X)
  ->
  struct
    let sub y = y - M.x
  end

module Sub10 = SubX (struct
  let x = 10
end)

module type T = sig
  type t

  val x : t
end

module Pair1 (M : T) = struct
  let p = (M.x, 1)
end

module P0 = Pair1 (struct
  type t = int

  let x = 0
end)

module PA = Pair1 (struct
  type t = char

  let x = 'a'
end)

module F (M : sig
  val x : int
end) =
struct
  let y = M.x
end

module X = struct
  let x = 0
end

module Z = struct
  let x = 0
  let z = 0
end

module FX = F (X)
module FZ = F (Z)

(* The Stdlib Map uses a functor called Make *)
module IntMap = Map.Make (Int)