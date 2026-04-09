open OUnit2
open Myqueue

let suite =
  "mqueue"
  >::: [
         ( "empty queue" >:: fun _ ->
           assert_equal [] BatchedQueue.(empty |> to_list) );
         ( "empty queue is empty" >:: fun _ ->
           assert_bool "empty queue was not empty" BatchedQueue.(empty |> is_empty)
         );
         ( "enqueue singleton" >:: fun _ ->
           assert_equal [ 1 ] BatchedQueue.(empty |> enqueue 1 |> to_list) );
         ( "enqueue multiple" >:: fun _ ->
           assert_equal [ 1; 2 ]
             BatchedQueue.(empty |> enqueue 1 |> enqueue 2 |> to_list) );
         ( "size of empty queue" >:: fun _ ->
           assert_equal 0 BatchedQueue.(empty |> size) );
         ( "size of non-empty queue" >:: fun _ ->
           assert_equal 5
             BatchedQueue.(
               empty |> enqueue 1 |> enqueue 2 |> enqueue 2 |> enqueue 4
               |> enqueue 5 |> size) );
         ( "dequeue non-empty" >:: fun _ ->
           assert_equal [ 2; 3 ]
             BatchedQueue.(
               empty |> enqueue 1 |> enqueue 2 |> enqueue 3 |> dequeue
               |> to_list) );
         ( "front, non-empty" >:: fun _ ->
           assert_equal 1 BatchedQueue.(empty |> enqueue 1 |> enqueue 2 |> front)
         );
       ]

let () = run_test_tt_main suite
