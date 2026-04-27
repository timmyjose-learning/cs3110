open OUnit2
open Values_and_functions.Chapter

let function_pattern_matching_suite =
  let robin = ("UK", { first = "Robin"; last = "Milner" }) in
  "function pattern matching"
  >::: [
         ( "get_country returns country" >:: fun _ ->
           assert_equal "UK" (get_country robin) );
         ( "get_first returns first name" >:: fun _ ->
           assert_equal "Robin" (get_first robin) );
         ( "get_last returns last name" >:: fun _ ->
           assert_equal "Milner" (get_last robin) );
       ]

let pattern_matching_suite =
  "pattern function" >::: [ function_pattern_matching_suite ]

let suite = "valus and functions" >::: [ pattern_matching_suite ]
let () = run_test_tt_main suite
