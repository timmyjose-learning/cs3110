type 'a t

exception Empty

val empty : 'a t
val is_empty : 'a t -> bool
val size : 'a t -> int
val push : 'a -> 'a t -> 'a t
val peek : 'a t -> 'a
val peek_opt : 'a t -> 'a option
val pop : 'a t -> 'a t
val pop_opt : 'a t -> 'a t option
val to_list : 'a t -> 'a list

(*
 *
 * In the Option module:
 * Option.map or fmap:
 *
  let (>>|) opt f = 
  match opt with
  None -> None
  Some x -> Some (f x)
 
 * Option.bind:
  let (>==) opt f = 
   match opt with
   None -> None
   | Some x -> f x
 *)
val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option