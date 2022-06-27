(* open Record_world *)

let%expect_test _ =
  print_endline "hi";
  [%expect {|hi|}]
