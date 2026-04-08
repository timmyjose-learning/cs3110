type 'a t = 'a list

exception Empty

let empty = []
let is_empty = function [] -> true | _ -> false
let size = List.length
let push x st = x :: st
let peek = function [] -> raise Empty | h :: _ -> h
let pop = function [] -> raise Empty | _ :: t -> t
let to_list = Fun.id
