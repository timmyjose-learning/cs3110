open OUnit2
open Ch5

let make_sum_test name expected input =
  name >:: fun _ -> assert_equal expected (Sum.sum input) ~printer:string_of_int

let make_next_weekday_test name expected input =
  name >:: fun _ ->
  assert_equal expected (Days.next_weekday input) ~printer:Days.string_of_day

let sum_tests =
  "test suite for sum"
  >::: [
         make_sum_test "empty" 0 [];
         make_sum_test "singleton" 1 [ 1 ];
         make_sum_test "multiple_elements" 15 [ 1; 2; 3; 4; 5 ];
       ]

let day_tests =
  "test siote for next_weekday"
  >::: [
         make_next_weekday_test "sunday" Days.Monday Days.Sunday;
         make_next_weekday_test "monday" Days.Tuesday Days.Monday;
         make_next_weekday_test "tuesday" Days.Wednesday Days.Tuesday;
         make_next_weekday_test "Wednesday" Days.Thursday Days.Wednesday;
         make_next_weekday_test "thursday" Days.Friday Days.Thursday;
         make_next_weekday_test "friday" Days.Monday Days.Friday;
         make_next_weekday_test "saturday" Days.Monday Days.Saturday;
       ]

let tests = "all tests" >::: [ sum_tests; day_tests ]
let _ = run_test_tt_main tests
