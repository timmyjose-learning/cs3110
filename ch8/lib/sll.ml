(* Single-linked list using mutability *)

type 'a node = { next : 'a mlist; value : 'a }
(** An ['a node] is a node of a mutable singly-linked list. It contains a value
    of type ['a] and a link to the [next] node. *)

and 'a mlist = 'a node option ref
(** An ['a mlist] is a mutable singly-linke list with elements of type ['a]. The
    [option] represents the possibility that the list is empty. *)

(** [empty] is an empty singly-linked list *)
let empty () : 'a mlist = ref None

(** [insert_first lst v] mutates mlist [lst] by inserting [v] as the first value
    of the list. *)
let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst := Some { next = ref !lst; value = v }

(** [to_list lst] is an OCaml list containing the same values and in the same
    order as the mlist [lst]. Not tail-recursive *)
let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with None -> [] | Some { next; value } -> value :: to_list next
