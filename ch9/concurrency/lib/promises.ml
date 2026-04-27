module type PROMISE = sig
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn
  type 'a promise
  type 'a resolver

  val make : unit -> 'a promise * 'a resolver
  val fulfill : 'a resolver -> 'a -> unit
  val reject : 'a resolver -> exn -> unit
  val state : 'a promise -> 'a state
  val return : 'a -> 'a promise
end

module Promise : PROMISE = struct
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn
  type 'a promise = 'a state ref
  type 'a resolver = 'a promise

  let write_once r x =
    if !r = Pending then r := x else invalid_arg "cannot write twice"

  let make () =
    let p = ref Pending in
    (p, p)

  let fulfill r x = write_once r (Fulfilled x)
  let reject r e = write_once r (Rejected e)
  let state p = !p
  let return x = ref (Fulfilled x)
end