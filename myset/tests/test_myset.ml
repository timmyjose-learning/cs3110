[@@@warning "-33"]

open OUnit2
open Myset

let uniq_list_set_suite =
  "UniqListSet"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] UniqListSet.(empty |> elements));
         ( "unique elements" >:: fun _ ->
           assert_equal [ 3; 2; 1 ]
             UniqListSet.(empty |> add 1 |> add 2 |> add 2 |> add 3 |> elements)
         );
         ( "mem - element present" >:: fun _ ->
           assert_bool "expected mem to be true"
             UniqListSet.(empty |> add 1 |> add 2 |> mem 1) );
         ( "mem - element not present" >:: fun _ ->
           assert_bool "expected mem not to be true"
             (not UniqListSet.(empty |> add 1 |> add 2 |> mem 11)) );
       ]

let list_set_suite =
  "ListSet"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] ListSet.(empty |> elements));
         ( "unique elements" >:: fun _ ->
               assert_equal [1; 2; 3]
             ListSet.(empty |> add 1 |> add 2 |> add 2 |> add 3 |> elements) );
         ( "mem - element present" >:: fun _ ->
           assert_bool "expected mem to be true"
             ListSet.(empty |> add 1 |> add 2 |> mem 1) );
         ( "mem - element not present" >:: fun _ ->
           assert_bool "expected mem not to be true"
             (not ListSet.(empty |> add 1 |> add 2 |> mem 11)) );
       ]

let suite = "set" >::: [ uniq_list_set_suite; list_set_suite ]
let () = run_test_tt_main suite
