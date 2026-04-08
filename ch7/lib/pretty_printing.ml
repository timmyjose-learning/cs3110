module type Stack = sig
  type 'a t

  exception Empty

  val empty : 'a t
  val size : 'a t -> int
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** pretty printer support *)
end

module ListStack : Stack = struct
  type 'a t = 'a list

  exception Empty

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let size = List.length
  let push x st = x :: st
  let peek = function [] -> raise Empty | h :: _ -> h
  let pop = function [] -> raise Empty | _ :: t -> t

  (** pretty-printer for ListStack *)
  let pp pp_val fmt s =
    let open Format in
    let pp_break fmt () = fprintf fmt "@," in
    fprintf fmt "@[<v 0>top of stack";
    if s <> [] then fprintf fmt "@,";
    pp_print_list ~pp_sep:pp_break pp_val fmt s;
    fprintf fmt "@,bottom of stack@]"
end

(*
 * #install_printer 
 * #remove_printer
 *)
