(* Pattern Matching *)

type name = { first : string; last : string }

let get_country (country, { first = _; last = _ }) = country
let get_first (_, { first; last = _ }) = first
let get_last (_, { first = _; last }) = last

type citizen = string * name

