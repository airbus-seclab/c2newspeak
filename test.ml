open Tap
open Range

let assert_inclusion a b name =
  assert_true name (a <=% b)

let range _ =
  test_plan 6;
  let intvl = from_bounds (-15) 23 in
  assert_inclusion bottom top    "bot <= top";  
  assert_inclusion bottom intvl  "bot <=  I ";
  assert_inclusion intvl  top    " I  <= top";
  assert_inclusion top    top    "top <= top";
  assert_inclusion intvl  intvl  " I  <= I  ";
  assert_inclusion bottom bottom "bot <= bot";
  test_end ()
