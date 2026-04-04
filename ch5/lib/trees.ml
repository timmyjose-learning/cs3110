(* Trees *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let t1 =
  Node
    ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)),
      4,
      Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )

let rec tree_to_list = function
  | Leaf -> []
  | Node (l, v, r) -> tree_to_list l @ [ v ] @ tree_to_list r

let rec size = function Leaf -> 0 | Node (l, _, r) -> size l + 1 + size r