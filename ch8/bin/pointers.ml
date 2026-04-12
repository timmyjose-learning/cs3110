[@@@warning "-32-34"]

type 'a pointer = 'a ref option

let null = None
let malloc x = Some (ref x)

exception Segfault

let deref = function None -> raise Segfault | Some loc -> !loc
let ( ~* ) = deref
let assign p x = match p with None -> raise Segfault | Some loc -> loc := x
let address = function None -> 0 | Some loc -> Obj.magic loc
let ( ~&  ) = address
