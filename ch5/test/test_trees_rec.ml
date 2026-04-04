open OUnit2
open Ch5

let tree_to_list_suite =
  "tree_to_list"
  >::: [
         ( "empty" >:: fun _ ->
           assert_equal [] (Trees_rec.tree_to_list Trees_rec.empty) );
         ( "singleton" >:: fun _ ->
           assert_equal [ 1 ] (Trees_rec.tree_to_list Trees_rec.singleton) );
         ( "multi-node" >:: fun _ ->
           assert_equal [ 1; 2; 3; 4; 5; 6; 7 ]
             (Trees_rec.tree_to_list Trees_rec.t1) );
       ]

let size_suite =
  "size"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (Trees_rec.size Trees_rec.empty));
         ( "singleton" >:: fun _ ->
           assert_equal 1 (Trees_rec.size Trees_rec.singleton) );
         ("multi-node" >:: fun _ -> assert_equal 7 (Trees_rec.size Trees_rec.t1));
       ]

let suite = "tree tests" >::: [ tree_to_list_suite; size_suite ]
let () = run_test_tt_main suite