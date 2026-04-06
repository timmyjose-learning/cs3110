let rec lst_and_rec = function [] -> true | h :: t -> h && lst_and_rec t
let lst_and_fold = List.fold_left (fun acc h -> acc && h) true
let lst_and_lib = List.for_all (fun x -> x)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec map_tree f = function
  | Leaf -> Leaf
  | Node (l, v, r) -> Node (map_tree f l, f v, map_tree f r)

let rec fold_tree f acc = function
  | Leaf -> acc
  | Node (l, v, r) -> f (fold_tree f acc l) v (fold_tree f acc r)

let t1 = Node (Node (Leaf, 2, Leaf), 5, Node (Leaf, 6, Leaf))
let size t = fold_tree (fun l _ r -> 1 + l + r) 0 t
let depth t = fold_tree (fun l _ r -> 1 + max l r) 0 t
let preorder t = fold_tree (fun l v r -> [ v ] @ l @ r) [] t
let inorder t = fold_tree (fun l v r -> l @ [ v ] @ r) [] t
let postorder t = fold_tree (fun l v r -> l @ r @ [ v ]) [] t

let rec filter_tree p = function
  | Leaf -> Leaf
  | Node (l, v, r) ->
      if p v then Node (filter_tree p l, v, filter_tree p r) else Leaf

(* Pipelining *)
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)
let square n = n * n
let sum = List.fold_left ( + ) 0
let sum_sq n = 0 -- n |> List.map square |> sum

(* Currying *)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let add x y = x + y
let uncurried_add = uncurry add
let add' (x, y) = x + y
let curried_add' = curry add'
