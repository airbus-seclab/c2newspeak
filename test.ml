open Tap
open Range

let assert_incl a b =
  assert_true (a <=% b)

let assert_not_incl a b =
  assert_false (a <=% b)

let range _ =
  test_plan 12;

  (*
   *    [---I---]
   *          [---J---]
   *  [---K--------------]
   *)
  let i = from_bounds (-15) 23 in
  let j = from_bounds   12  26 in
  let k = from_bounds (-25) 35 in

  (* Inclusion *)
  assert_incl bottom  top   "bot <= top";  
  assert_incl bottom   i    "bot <=  I ";
  assert_incl   i     top   " I  <= top";
  assert_incl  top    top   "top <= top";
  assert_incl   i      i    " I  <=  I ";
  assert_incl bottom bottom "bot <= bot";
  assert_incl   i      k    " I  <=  K ";
  assert_incl   j      k    " J  <=  K ";
  assert_not_incl i    j    " I  </=  J ";
  assert_not_incl j    i    " J  </=  I ";

  (* Join *)
  assert_equal (join i k) k "I \\/ K = K";
  assert_equal (join i j) (from_bounds (-15) 26) "I \\/ J";

  test_end ()
