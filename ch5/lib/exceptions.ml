(* Exceptions *)

exception A
exception B
exception Code of int
exception Details of string

let raise_exception = function
  | 1 -> raise A
  | 2 -> raise B
  | 3 -> raise (Code 42)
  | 4 -> raise (Details "something wrong")
  | code -> raise (Invalid_argument (string_of_int code))

let exception_demo () =
  let code = read_int () in
  try raise_exception code with
  | A -> print_endline "Error A"
  | B -> print_endline "Error B"
  | Code ret -> print_endline ("Code: " ^ string_of_int ret)
  | Details err -> print_endline err
;;

(* pattern matching for execptions *)
match List.hd [] with
| [] -> "empty"
| _ :: _ -> "non-empty"
| exception Failure s -> s