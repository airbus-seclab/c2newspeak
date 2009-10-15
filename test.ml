open Tap
open Range

let assert_incl a b =
  assert_true (a <=% b)

let assert_not_incl a b =
  assert_false (a <=% b)

let range _ =
  test_plan 34;

  (*
   *               [-L-]
   *    [---I---]
   *          [---J---]
   *  [---K--------------]
   *)
  let i = from_bounds (-15) 23 in
  let j = from_bounds   12  26 in
  let k = from_bounds (-25) 35 in
  let l = from_bounds   24  30 in

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
  assert_equal (join i l) (from_bounds (-15) 30) "I \\/ L";

  (* FIXME + infinite cases *)

  (* Meet *)

  (* FIXME + infinite cases *)
  (* FIXME + nondegenerate cases *)
  let m = -25 and a = -5 and
      p = 0   and q = 8  and
      b = 13  and n = 42 in
  let fb = from_bounds in
                      (*  m      a   p   q   b      n      *)
                      (*                                   *)
  let  r  = fb a b in (*         [-----R-----]             *)
  let q01 = fb m m in (*  | Q1                             *)
  let q02 = fb m a in (*  [-Q2---]                         *)
  let q03 = fb m p in (*  [-Q3-------]                     *)
  let q04 = fb m q in (*  [-Q4-----------]                 *)
  let q05 = fb m b in (*  [-Q5---------------]             *)
  let q06 = fb m n in (*  [-Q6----------------------]      *)
  let q07 = fb a a in (*         | Q7                      *)
  let q08 = fb a p in (*         [Q8]                      *)
  let q09 = fb a q in (*         [-Q9----]                 *)
  let q10 = fb a b in (*         [-Q10-------]             *)
  let q11 = fb a n in (*         [-Q11--------------]      *)
  let q12 = fb p p in (*            | Q12                  *)
  let q13 = fb p q in (*            [Q13-]                 *)
  let q14 = fb p b in (*            [-Q14----]             *)
  let q15 = fb p n in (*            [-Q15-----------]      *)
  let q16 = fb q q in (*                 | Q16             *)
  let q17 = fb q b in (*                 [Q17]             *)
  let q18 = fb q n in (*                 [-Q18------]      *)
  let q19 = fb b b in (*                     | Q19         *)
  let q20 = fb b n in (*                     [-Q20--]      *)
  let q21 = fb n n in (*                            | Q21  *)

  assert_equal (meet r q01) bottom "q01";
  assert_equal (meet r q02) q07    "q02";
  assert_equal (meet r q03) q08    "q03";
  assert_equal (meet r q04) q09    "q04";
  assert_equal (meet r q05) r      "q05";
  assert_equal (meet r q06) r      "q06";
  assert_equal (meet r q07) q07    "q07";
  assert_equal (meet r q08) q08    "q08";
  assert_equal (meet r q09) q09    "q09";
  assert_equal (meet r q10) q10    "q10";
  assert_equal (meet r q11) q10    "q11";
  assert_equal (meet r q12) q12    "q12";
  assert_equal (meet r q13) q13    "q13";
  assert_equal (meet r q14) q14    "q14";
  assert_equal (meet r q15) q14    "q15";
  assert_equal (meet r q16) q16    "q16";
  assert_equal (meet r q17) q17    "q17";
  assert_equal (meet r q18) q17    "q18";
  assert_equal (meet r q19) q19    "q19";
  assert_equal (meet r q20) q19    "q20";
  assert_equal (meet r q21) bottom "q21";

  test_end ()
