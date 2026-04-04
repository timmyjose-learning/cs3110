(* Trees using records *)

type 'a tree = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a tree; right : 'a tree }

let empty = Leaf
let singleton = Node { value = 1; left = Leaf; right = Leaf }

let t1 =
  Node
    {
      value = 4;
      left =
        Node
          {
            value = 2;
            left = Node { value = 1; left = Leaf; right = Leaf };
            right = Node { value = 3; left = Leaf; right = Leaf };
          };
      right =
        Node
          {
            value = 6;
            left = Node { value = 5; left = Leaf; right = Leaf };
            right = Node { value = 7; left = Leaf; right = Leaf };
          };
    }

let rec tree_to_list = function
  | Leaf -> []
  | Node { value; left; right } ->
      tree_to_list left @ [ value ] @ tree_to_list right

let rec size = function
  | Leaf -> 0
  | Node { value = _; left; right } -> 1 + size left + size right

(** [mem x t] detects whether [x] exists in the tree [t] or not *)

let rec mem x = function
  | Leaf -> false
  | Node { value; left; right } ->
      if value < x then mem x right else if value > x then mem x left else true

let rec preorder = function
  | Leaf -> []
  | Node { value; left; right } -> [ value ] @ preorder left @ preorder right

let preorder_lin t =
  let rec aux acc = function
    | Leaf -> acc
    | Node { value; left; right } -> value :: aux (aux acc right) left
  in
  aux [] t

let rec inorder = function
  | Leaf -> []
  | Node { value; left; right } -> inorder left @ [ value ] @ inorder right

let inorder_lin t =
  let rec aux acc = function
    | Leaf -> acc
    | Node { value; left; right } -> aux (value :: aux acc left) right
  in
  List. rev (aux [] t)

let rec postorder = function
  | Leaf -> []
  | Node { value; left; right } -> postorder left @ postorder right @ [ value ]

let postorder_lin t =
  let rec aux acc = function
    | Leaf -> acc
    | Node { value; left; right } -> value :: aux (aux acc left) right
  in
  List.rev (aux [] t)