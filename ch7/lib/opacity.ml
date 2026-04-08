(* we can expose the definition of a type in the signature, so abstract types are really more of a case of opacity than anything else *)

module type T = sig
  type t = int

  val x : t
end

module M : T = struct
  type t = int

  let x = 42
end

let a : int = M.x
