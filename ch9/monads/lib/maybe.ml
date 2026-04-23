(*
 let return (x : int) : int option = Some x

let bind (x : int option) (op : int -> int option) : int option =
  match x with None -> None | Some a -> op a

let ( >>= ) = bind

let ( + ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( + ) a b)

let ( - ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( - ) a b)

let ( * ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( * ) a b)

let ( / ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> if b = 0 then None else return (Stdlib.( / ) a b)

let _ = Some 1 + (Some 4 / Some 2)
*)

(* The actual Maybe monad *)

open Monad

module Maybe : Monad with type 'a t = 'a option = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >>= ) x f = match x with None -> None | Some a -> f a
end

let ( + ) (x : int Maybe.t) (y : int Maybe.t) : int Maybe.t =
  Maybe.( >>= ) x (fun a ->
      Maybe.( >>= ) y (fun b -> Maybe.return (Stdlib.( + ) a b)))

let ( - ) (x : int Maybe.t) (y : int Maybe.t) : int Maybe.t =
  Maybe.( >>= ) x (fun a ->
      Maybe.( >>= ) y (fun b -> Maybe.return (Stdlib.( - ) a b)))

let ( * ) (x : int Maybe.t) (y : int Maybe.t) : int Maybe.t =
  Maybe.( >>= ) x (fun a ->
      Maybe.( >>= ) y (fun b -> Maybe.return (Stdlib.( * ) a b)))

let ( / ) (x : int Maybe.t) (y : int Maybe.t) : int Maybe.t =
  Maybe.( >>= ) x (fun a ->
      Maybe.( >>= ) y (fun b ->
          if b = 0 then None else Maybe.return (Stdlib.( / ) a b)))

let _ = Some 1 + (Some 4 / Some 2)