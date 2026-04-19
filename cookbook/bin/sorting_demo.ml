let l = [ 1; 2; 5; 3; 5; 4; -1 ]
let l' = List.sort compare l;;

l'
|> List.iter (fun d ->
    print_int d;
    print_newline ())
;;

(* Arrays are mutable, in-place, unlike lists *)
let arr = [| 1; 2; 5; 2; 1; 3; 4; 100; -100 |]
let () = Array.sort compare arr;;

arr
|> Array.iter (fun d ->
    print_int d;
    print_newline ())
;;

let compare_inssenstive s1 s2 =
  compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)

let words = [| "hello"; "world"; "again"; "we"; "meet" |]
let () = Array.sort compare_inssenstive words;;

words |> Array.iter (fun s -> print_endline s)