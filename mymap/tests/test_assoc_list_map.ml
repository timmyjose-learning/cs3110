open OUnit2
open Mymap

let suite =
  "assoc list maps"
  >::: [
         ( "empty map" >:: fun _ ->
           assert_equal [] AssocListMap.(empty |> bindings) );
         ( "singleton map" >:: fun _ ->
           assert_equal
             [ (1, "one") ]
             AssocListMap.(empty |> insert 1 "one" |> bindings) );
         ( "multi-entry map" >:: fun _ ->
           assert_equal
             [ (1, "one"); (2, "two"); (3, "three") ]
             AssocListMap.(
               empty |> insert 3 "three" |> insert 1 "one" |> insert 2 "two"
               |> bindings) );
         ( "lookup" >:: fun _ ->
           assert_equal "five"
             AssocListMap.(
               empty |> insert 1 "one" |> insert 2 "two" |> insert 5 "five"
               |> insert 3 "three" |> insert 4 "four" |> lookup 5) );
         ( "lookup non-present key" >:: fun _ ->
           assert_raises Not_found (fun () ->
               AssocListMap.(empty |> insert 1 "one" |> lookup 2)) );
       ]

let () = run_test_tt_main suite