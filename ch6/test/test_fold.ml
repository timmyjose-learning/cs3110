open OUnit2
open Ch6

let fold_left_length_suite =
  "fold_left_length"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (Fold.length_left []));
         ("singleton" >:: fun _ -> assert_equal 1 (Fold.length_left [ 1 ]));
         ( "multiple elements" >:: fun _ ->
           assert_equal 5 (Fold.length_left [ 1; 2; 3; 4; 5 ]) );
       ]

let fold_left_rev_suite =
  "fold_left_rev"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Fold.rev_left []));
         ("singleton" >:: fun _ -> assert_equal [ 1 ] (Fold.rev_left [ 1 ]));
         ( "multiple elements" >:: fun _ ->
           assert_equal [ 5; 4; 3; 2; 1 ] (Fold.rev_left [ 1; 2; 3; 4; 5 ]) );
       ]

let fold_left_map_suite =
  "fold_left_map"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Fold.map_left (fun x -> x + 1) []) );
         ( "singleton" >:: fun _ ->
           assert_equal [ 2 ] (Fold.map_left (fun x -> x + 1) [ 1 ]) );
         ( "multiple elements" >:: fun _ ->
           assert_equal [ 2; 3; 4; 5; 6 ]
             (Fold.map_left (fun x -> x + 1) [ 1; 2; 3; 4; 5 ]) );
       ]

let fold_left_filter_suite =
  "fold_left_filter"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Fold.filter_left (fun x -> x mod 2 = 0) []) );
         ( "singleton" >:: fun _ ->
           assert_equal [] (Fold.filter_left (fun x -> x mod 2 = 0) [ 1 ]) );
         ( "multiple elements" >:: fun _ ->
           assert_equal [ 2; 4 ]
             (Fold.filter_left (fun x -> x mod 2 = 0) [ 1; 2; 3; 4; 5 ]) );
       ]

let fold_left_suite =
  "fold_left"
  >::: [
         fold_left_length_suite;
         fold_left_rev_suite;
         fold_left_map_suite;
         fold_left_filter_suite;
       ]

let fold_right_length_suite =
  "fold_right_length"
  >::: [ ("empty" >:: fun _ -> assert_equal 0 (Fold.length_right [])) ]

let fold_right_rev_suite =
  "fold_right_rev"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Fold.rev_right []));
         ("singleton" >:: fun _ -> assert_equal [ 1 ] (Fold.rev_right [ 1 ]));
         ( "multiple elements" >:: fun _ ->
           assert_equal [ 5; 4; 3; 2; 1 ] (Fold.rev_right [ 1; 2; 3; 4; 5 ]) );
       ]

let fold_right_map_suite =
  "fold_left_map"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Fold.map_right (fun x -> x + 1) []) );
         ( "singleton" >:: fun _ ->
           assert_equal [ 2 ] (Fold.map_right (fun x -> x + 1) [ 1 ]) );
         ( "multiple elements" >:: fun _ ->
           assert_equal [ 2; 3; 4; 5; 6 ]
             (Fold.map_right (fun x -> x + 1) [ 1; 2; 3; 4; 5 ]) );
       ]

let fold_right_filter_suite =
  "fold_right_filter"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Fold.filter_right (fun x -> x mod 2 = 0) []) );
         ( "singleton" >:: fun _ ->
           assert_equal [] (Fold.filter_right (fun x -> x mod 2 = 0) [ 1 ]) );
         ( "multiple elements" >:: fun _ ->
           assert_equal [ 2; 4 ]
             (Fold.filter_right (fun x -> x mod 2 = 0) [ 1; 2; 3; 4; 5 ]) );
       ]

let fold_right_suite =
  "fold_right"
  >::: [
         fold_right_length_suite;
         fold_right_rev_suite;
         fold_right_map_suite;
         fold_right_filter_suite;
       ]

let suite = "all tests" >::: [ fold_left_suite; fold_right_suite ]
let () = run_test_tt_main suite
