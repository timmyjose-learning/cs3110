module type MutableStack = sig
  type 'a t

  exception Empty

  val empty : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val peek : 'a t -> 'a
  val pop : 'a t -> unit
end

module MutableRecordStack : MutableStack = struct
  type 'a node = { value : 'a; next : 'a node option }
  type 'a t = { mutable top : 'a node option }

  exception Empty

  let empty () = { top = None }
  let push x s = s.top <- Some { value = x; next = s.top }

  let peek s =
    match s.top with None -> raise Empty | Some { value; _ } -> value

  let pop s =
    match s.top with None -> raise Empty | Some { next; _ } -> s.top <- next
end
