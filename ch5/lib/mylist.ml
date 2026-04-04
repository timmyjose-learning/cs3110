type 'a list = Nil | Cons of 'a * 'a list

let rec mylist_sum = function Nil -> 0 | Cons (h, t) -> h + mylist_sum t
