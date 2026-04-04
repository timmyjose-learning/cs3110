open OUnit2
open Ch5

let tree_to_list_suite =
  "tree_to_list"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (Trees.tree_to_list Leaf));
         ( "singleton" >:: fun _ ->
           assert_equal [ 1 ] (Trees.tree_to_list (Node (Leaf, 1, Leaf))) );
         ( "multi-node" >:: fun _ ->
           assert_equal [ 1; 2; 3; 4; 5; 6; 7 ] (Trees.tree_to_list Trees.t1) );
       ]

let size_suite =
  "size"
  >::: [
         ("empty" >:: fun _ -> assert_equal 0 (Trees.size Leaf));
         ( "singloton" >:: fun _ ->
           assert_equal 1 (Trees.size (Node (Leaf, 1, Leaf))) );
         ("multi-node" >:: fun _ -> assert_equal 7 (Trees.size Trees.t1));
       ]

let suite = "tree tests" >::: [ tree_to_list_suite; size_suite ]
let () = run_test_tt_main suite