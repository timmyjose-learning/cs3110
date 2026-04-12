(* Mutable single-linked lists using `mutable` instead of direct refs *)

type 'a node = { mutable next : 'a node option; value : 'a }
type 'a mlist = { mutable first : 'a node option }

let empty () : 'a mlist = { first = None }

let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst.first <- Some { next = lst.first; value = v }

let to_list (lst : 'a mlist) : 'a list = 
  let rec aux = function
      None -> []
    | Some { next ; value } -> value :: aux next
  in
  aux lst.first
