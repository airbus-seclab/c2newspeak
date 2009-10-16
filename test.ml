open Tap
open Range

let assert_incl a b =
  assert_true (a <=% b)

let assert_not_incl a b =
  assert_false (a <=% b)

let range _ =
  test_plan 65;

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

  let bot = bottom in
  let tc r q m =
    let name = to_string r ^" /\\ "
              ^to_string q ^" == "
              ^to_string m in
    assert_equal ~printer:to_string (meet r q) m name
  in

  let i = min_int in
  let m = -22 in
  let m' = -20 in
  let a = -8 in
  let p = 2  in
  let q = 7  in
  let b = 55 in
  let n = 123 in
  let n' = 1503 in
  let j = max_int in

  let z = from_bounds in

  (* R = {} *)
  (*  -oo          a         b         +oo  *)
  (*                                        *) tc bot   bot   bot;
  (*  -------------]                        *) tc bot (z i a) bot;
  (*  ------------------------------------  *) tc bot (z i j) bot;
  (*               |                        *) tc bot (z a a) bot;
  (*               [---------]              *) tc bot (z a b) bot;
  (*               [----------------------  *) tc bot (z a j) bot;

  (* R = {p} *)
  (*  -oo   m    a     p     b    n    +oo  *)
  (*                   | R                  *)
  (*                                        *) tc (z p p)   bot     bot;
  (*  -----------]                          *) tc (z p p) (z i a)   bot;
  (*  -----------------]                    *) tc (z p p) (z i p) (z p p);
  (*  -----------------------]              *) tc (z p p) (z i b) (z p p);
  (*  ------------------------------------  *) tc (z p p) (z i j) (z p p);
  (*        |                               *) tc (z p p) (z m m)   bot;
  (*        [----]                          *) tc (z p p) (z m a)   bot;
  (*        [----------]                    *) tc (z p p) (z m p) (z p p);
  (*        [----------------]              *) tc (z p p) (z m b) (z p p);
  (*        [-----------------------------  *) tc (z p p) (z m j) (z p p);
  (*                   |                    *) tc (z p p) (z p p) (z p p);
  (*                   [-----]              *) tc (z p p) (z p b) (z p p);
  (*                   [------------------  *) tc (z p p) (z p j) (z p p);
  (*                         |              *) tc (z p p) (z b b)   bot;
  (*                         [----]         *) tc (z p p) (z b n)   bot;
  (*                         [------------  *) tc (z p p) (z b j)   bot;

  (* R = [a;b] *)
  (* -oo  m  m'  a   p   q   b   n   n' +oo *)
  (*                                        *)
  (*             [-----R-----]              *)
  (*                                        *) tc (z a b)   bot     bot;
  (* -----]                                 *) tc (z a b) (z i m)   bot;
  (* ------------]                          *) tc (z a b) (z i a) (z a a);
  (* ----------------]                      *) tc (z a b) (z i p) (z a p);
  (* ------------------------]              *) tc (z a b) (z i b) (z a b);
  (* ----------------------------]          *) tc (z a b) (z i n) (z a b);
  (* -------------------------------------  *) tc (z a b) (z i j) (z a b);
  (*      |                                 *) tc (z a b) (z m m)   bot;
  (*      [--]                              *) tc (z a b) (z m m')  bot;
  (*      [------]                          *) tc (z a b) (z m a) (z a a);
  (*      [----------]                      *) tc (z a b) (z m p) (z a p);
  (*      [------------------]              *) tc (z a b) (z m b) (z a b);
  (*      [----------------------]          *) tc (z a b) (z m n) (z a b);
  (*      [-------------------------------  *) tc (z a b) (z m j) (z a b);
  (*             |                          *) tc (z a b) (z a a) (z a a);
  (*             [---]                      *) tc (z a b) (z a p) (z a p);
  (*             [-----------]              *) tc (z a b) (z a b) (z a b);
  (*             [---------------]          *) tc (z a b) (z a n) (z a b);
  (*             [------------------------  *) tc (z a b) (z a j) (z a b);
  (*                 |                      *) tc (z a b) (z p p) (z p p);
  (*                 [---]                  *) tc (z a b) (z p q) (z p q);
  (*                 [-------]              *) tc (z a b) (z p b) (z p b);
  (*                 [-----------]          *) tc (z a b) (z p n) (z p b);
  (*                 [--------------------  *) tc (z a b) (z p j) (z p b);
  (*                         |              *) tc (z a b) (z b b) (z b b);
  (*                         [---]          *) tc (z a b) (z b n) (z b b);
  (*                         [------------  *) tc (z a b) (z b j) (z b b);
  (*                             |          *) tc (z a b) (z n n)  bot;
  (*                             [---]      *) tc (z a b) (z n n') bot;
  (*                             [--------  *) tc (z a b) (z n j)  bot;

  test_end ()
