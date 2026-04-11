open OUnit2
open Functors

module StackTest (S : Stack.Stack) = struct
  let tests =
    [
      ( "peek (push x empty) = x" >:: fun _ ->
        assert_equal 1 S.(empty |> push 1 |> peek) );
    ]
end

module ListStackTester = StackTest (Stack.ListStack)
module VariantStackTester = StackTest (Stack.VariantStack)

let all_tests = List.flatten [ ListStackTester.tests; VariantStackTester.tests ]
let suite = "all test" >::: all_tests
let () = run_test_tt_main suite