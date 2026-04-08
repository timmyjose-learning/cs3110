[@@@warning "-32"]

type 'a t = 'a list

exception Empty

let empty = []
let is_empty = function [] -> true | _ -> false
let size = List.length
let push = List.cons
let peek = function [] -> raise Empty | h :: _ -> h
let peek_opt = function [] -> None | h :: _ -> Some h
let pop = function [] -> raise Empty | _ :: t -> t
let pop_opt = function [] -> None | _ :: t -> Some t
let to_list = Fun.id

(* Option.map / fmap *)
let ( >>| ) opt f = match opt with None -> None | Some x -> Some (f x)

(* Option.bind *)
let ( >>= ) opt f = match opt with None -> None | Some x -> f x
