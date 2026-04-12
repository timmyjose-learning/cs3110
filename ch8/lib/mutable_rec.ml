(* Mutable fields *)
type point = { x : int; y : int; mutable c : string }

(* References in OCaml are basically records with a single mutable field called `contents`. They are implemented in the C runtime. However, we can implement them in OCaml itself. *)
type 'a myref = { mutable contents : 'a }

let myref x = { contents = x }
let ( ! ) r = r.contents
let ( := ) r v = r.contents <- v
