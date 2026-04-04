let rec list_max = function
  | [] -> None
  | h :: t -> (
      match list_max t with None -> Some h | Some k -> Some (max h k))

let () = assert (None = list_max [])
let () = assert (Some 1 = list_max [ 1 ])
let () = assert (Some 5 = list_max [2; 5; 1; 3; 4])
