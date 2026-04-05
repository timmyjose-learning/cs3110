let rec fold_left f (acc : 'acc) = function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let rec fold_right f lst (acc : 'acc) =
  match lst with [] -> acc | h :: t -> f h (fold_right f t acc)

let length_left lst = fold_left (fun acc _ -> acc + 1) 0 lst
let length_right lst = fold_right (fun _ acc -> acc + 1) lst 0
let rev_left lst = fold_left (fun acc h -> h :: acc) [] lst
let rev_right lst = fold_right (fun h acc -> acc @ [ h ]) lst []
let map_left f lst = fold_left (fun acc h -> acc @ [ f h ]) [] lst
let map_right f lst = fold_right (fun h acc -> f h :: acc) lst []

let filter_left p lst =
  fold_left (fun acc h -> if p h then acc @ [ h ] else acc) [] lst

let filter_right p lst =
  fold_right (fun h acc -> if p h then h :: acc else acc) lst []
