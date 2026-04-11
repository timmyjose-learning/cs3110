open OUnit2
open Fset

module SetTester (S : Set.Set) = struct
  let tests =
    [
      ( "empty |> add 1 = [1]" >:: fun _ ->
        assert_equal [ 1 ] S.(empty |> add 1 |> elements) );
    ]
end

module SetLTester = SetTester (Set.SetL)
module UniqSetLTester = SetTester (Set.UniqSetL)

let suite =
  "all tests" >::: List.flatten [ SetLTester.tests; UniqSetLTester.tests ]

let () = run_test_tt_main suite
