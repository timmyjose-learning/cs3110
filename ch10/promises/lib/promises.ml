(** Simulating Lwt's promise types *)
module type PROMISE = sig
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn
  type 'a promise
  type 'a resolver

  val make : unit -> 'a promise * 'a resolver
  (** [make ()] is a new promise and a new resolver. The promise is pending *)

  val return : 'a -> 'a promise
  (** [return x] is a new promise that is already resolved with value [x] *)

  val state : 'a promise -> 'a state
  (** [state p] is the state of the promise p *)

  val fulfill : 'a resolver -> 'a -> unit
  (** [fulfill r x] fulfills the promise associated with the resolver [r] with
      value [x], meaning that [state p] will return [Fulfilled x]. Requires: [p]
      is pending *)

  val reject : 'a resolver -> exn -> unit
  (** [reject r e] rejects the promise associated with the resolver [r] with
      exception [e], meaning that [state p] will return [Rejected e]. Requires:
      [p] is pending *)
end

module Promise : PROMISE = struct
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn
  type 'a promise = 'a state ref
  type 'a resolver = 'a promise

  let write_once p s =
    if !p = Pending then p := s else invalid_arg "cannot write more than once"

  let make () =
    let p = ref Pending in
    (p, p)

  let return x = ref (Fulfilled x)
  let state p = !p
  let fulfill r x = write_once r (Fulfilled x)
  let reject r e = write_once r (Rejected e)
end