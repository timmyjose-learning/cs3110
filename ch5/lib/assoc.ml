(* Association Lists *)

let d = [ ("rectangle", 4); ("nonagon", 9); ("icosagon", 20) ]

(** [insert k v lst] inserts a new mapping from [k] to [v] into the association
    list [lst] *)
let insert k v lst = (k, v) :: lst

(** [lokkup k lst] searches for key [k] in the association list [lst]. If found,
    returns Some [v] else None *)
let rec lookup k = function
  | [] -> None
  | (kk, vv) :: t -> if kk = k then Some vv else lookup k t