open OUnit2

let suite =
  "testing exceptions suite"
  >::: [
         ( "empty list" >:: fun _ ->
           assert_raises (Failure "hd") (fun () -> List.hd []) );
       ]

let () = run_test_tt_main suite