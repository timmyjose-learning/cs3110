(* Modular Programming *)

(* A struct is a collection of definitions *)

(*
 * struct 
  let inc x = x + 1
  type primary_color = Red | Green | Blue
  exception Oops
end
*)

module MyModule = struct
  let inc x = x + 1

  type primary_color = Red | Green | Blue

  exception Oops
end

module E = struct end

module M = struct
  let rec even = function 0 -> true | n -> odd (n - 1)
  and odd = function 0 -> false | n -> even (n - 1)
end

(* Scope and Open *)

module M1 = struct
  let x = 42
end

(* Use `open M` to bring all the definitions of module `M` into the current scope *)

(** [lower_trim s] is [s] in lower case with whitespace removed *)
let lower_trim s =
  let open String in
  s |> trim |> lowercase_ascii

module M2 = struct
  (** [uppercase_all lst] upper cases all the strings in [lst] *)
  let uppercase_all = List.map String.uppercase_ascii
end

(* nested modules *)

module type X = sig
  val x : int
end

module type T = sig
  module Inner : X
end

module NestedModule : T = struct
  module Inner : X = struct
    let x : int = 42
  end
end

(* module subtyping *)

module M3 = struct
  let x = 0
  let z = 0
end

module type TX = sig
  val x : int
end

module M3X : TX = M3

module type TZ = sig
  val z : int
end

module M3Z : TZ = M3

module type M3T = sig
  val x : int
  val z : int
end

module M3XZ : M3T = M3

module type IntFun = sig
  val f : int -> int
end

module IdFun = struct
  let f x = x
end

module IId : IntFun = IdFun

(* encapsulation *)

module type MY_MATH = sig
  val fact : int -> int
end

module MyMath : MY_MATH = struct
  let rec fact_aux acc n = if n = 0 then acc else fact_aux (acc * n) (n - 1)
  let fact n = fact_aux 1 n
end



