open Format

let print_dict = Hashtbl.iter (fun k v -> printf "%i => %s\n" k v);;

let numbers = Hashtbl.create (-1) in
Hashtbl.add numbers 1 "Eins";
Hashtbl.add numbers 2 "Zwei";
Hashtbl.add numbers 3 "Drei";
Hashtbl.add numbers 4 "Vier";
Hashtbl.add numbers 5 "Fuenf";
printf "%s\n%!" (Hashtbl.find numbers 1);
print_dict numbers;

try printf "%s\n" (Hashtbl.find numbers 100)
with Not_found ->
  (((printf "Could not find key: %i\n" 100;

     try printf "%s\n" (Hashtbl.find numbers 5)
     with Not_found -> printf "Could not find key 5\n");

    match Hashtbl.find_opt numbers 100 with
    | None -> printf "Could not find key 100\n"
    | Some s -> printf "%s\n" s);

   match Hashtbl.find_opt numbers 5 with
   | None -> printf "Could not find key 5\n"
   | Some s -> printf "%s\n" s);

  (* This is interesting behaviour - because OCaml hash tables add values as a list against a key, deleting the key will return the next value in the value list, if present *)
  Hashtbl.add numbers 2 "Zweiiiiii";
  print_dict numbers;
  print_newline ();
  Hashtbl.remove numbers 2;
  print_dict numbers