open Tap
open Range

let assert_incl a b =
  assert_true (a <=% b)

let assert_not_incl a b =
  assert_false (a <=% b)

let range _ =
  test_plan 190;

  (*
   *               [-L-]
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

  (* Join + Meet *)

  let bot = bottom in
  let tc r q m j =
    let name_m = to_string r ^" /\\ "
               ^ to_string q ^" == "
               ^ to_string m in
    let name_j = to_string r ^" \\/ "
               ^ to_string q ^" == "
               ^ to_string j in
    assert_equal ~printer:to_string (meet r q) m name_m;
    assert_equal ~printer:to_string (join r q) j name_j;
  in


  let i = min_int in let m = -22 in let m' = -20  in
  let a = -8      in let p = 2   in let q  = 7    in
  let b = 55      in let n = 123 in let n' = 1503 in
  let j = max_int in

  let z = from_bounds in

  (* R = {} *)
  (*  -oo          a         b         +oo  *)
  (*                                        *) tc   bot     bot     bot     bot;
  (*  -------------]                        *) tc   bot   (z i a)   bot   (z i a);
  (*  ------------------------------------  *) tc   bot   (z i j)   bot   (z i j);
  (*               |                        *) tc   bot   (z a a)   bot   (z a a);
  (*               [---------]              *) tc   bot   (z a b)   bot   (z a b);
  (*               [----------------------  *) tc   bot   (z a j)   bot   (z a j);

  (* R = [-oo;p] *)
  (*  -oo    m    a   p        b   n   +oo  *)
  (*  ----------------] R                   *)
  (*                                        *)
  (*                                        *) tc (z i p)   bot     bot   (z i p);
  (*  -------]                              *) tc (z i p) (z i m) (z i m) (z i p);
  (*  ----------------]                     *) tc (z i p) (z i p) (z i p) (z i p);
  (*  -------------------------]            *) tc (z i p) (z i b) (z i p) (z i b);
  (*  ------------------------------------  *) tc (z i p) (z i j) (z i p) (z i j);
  (*         |                              *) tc (z i p) (z m m) (z m m) (z i p);
  (*         [----]                         *) tc (z i p) (z m a) (z m a) (z i p);
  (*         [--------]                     *) tc (z i p) (z m p) (z m p) (z i p);
  (*         [-----------------]            *) tc (z i p) (z m b) (z m p) (z i b);
  (*         [----------------------------  *) tc (z i p) (z m j) (z m p) (z i j);
  (*                  |                     *) tc (z i p) (z p p) (z p p) (z i p);
  (*                  [--------]            *) tc (z i p) (z p b) (z p p) (z i b);
  (*                  [-------------------  *) tc (z i p) (z p j) (z p p) (z i j);
  (*                           |            *) tc (z i p) (z b b)   bot   (z i b);
  (*                           [---]        *) tc (z i p) (z b n)   bot   (z i n);
  (*                           [----------  *) tc (z i p) (z b j)   bot   (z i j);

  (* R = [-oo;+oo] *)
  (*  -oo          a         b         +oo  *)
  (*                                        *) tc   top     top     top     top;
  (*  -------------]                        *) tc   top   (z i a) (z i a)   top;
  (*  ------------------------------------  *) tc   top   (z i j) (z i j)   top;
  (*               |                        *) tc   top   (z a a) (z a a)   top;
  (*               [---------]              *) tc   top   (z a b) (z a b)   top;
  (*               [----------------------  *) tc   top   (z a j) (z a j)   top;


  (* R = {p} *)
  (*  -oo   m    a     p     b    n    +oo  *)
  (*                   | R                  *)
  (*                                        *)
  (*                                        *) tc (z p p)   bot     bot   (z p p);
  (*  -----------]                          *) tc (z p p) (z i a)   bot   (z i p);
  (*  -----------------]                    *) tc (z p p) (z i p) (z p p) (z i p);
  (*  -----------------------]              *) tc (z p p) (z i b) (z p p) (z i b);
  (*  ------------------------------------  *) tc (z p p) (z i j) (z p p) (z i j);
  (*        |                               *) tc (z p p) (z m m)   bot   (z m p);
  (*        [----]                          *) tc (z p p) (z m a)   bot   (z m p);
  (*        [----------]                    *) tc (z p p) (z m p) (z p p) (z m p);
  (*        [----------------]              *) tc (z p p) (z m b) (z p p) (z m b);
  (*        [-----------------------------  *) tc (z p p) (z m j) (z p p) (z m j);
  (*                   |                    *) tc (z p p) (z p p) (z p p) (z p p);
  (*                   [-----]              *) tc (z p p) (z p b) (z p p) (z p b);
  (*                   [------------------  *) tc (z p p) (z p j) (z p p) (z p j);
  (*                         |              *) tc (z p p) (z b b)   bot   (z p b);
  (*                         [----]         *) tc (z p p) (z b n)   bot   (z p n);
  (*                         [------------  *) tc (z p p) (z b j)   bot   (z p j);

  (* R = [a;b] *)
  (* -oo  m  m'  a   p   q   b   n   n' +oo *)
  (*             [-----R-----]              *)
  (*                                        *)
  (*                                        *) tc (z a b)   bot     bot   (z a b);
  (* -----]                                 *) tc (z a b) (z i m)   bot   (z i b);
  (* ------------]                          *) tc (z a b) (z i a) (z a a) (z i b);
  (* ----------------]                      *) tc (z a b) (z i p) (z a p) (z i b);
  (* ------------------------]              *) tc (z a b) (z i b) (z a b) (z i b);
  (* ----------------------------]          *) tc (z a b) (z i n) (z a b) (z i n);
  (* -------------------------------------  *) tc (z a b) (z i j) (z a b) (z i j);
  (*      |                                 *) tc (z a b) (z m m)   bot   (z m b);
  (*      [--]                              *) tc (z a b) (z m m')  bot   (z m b);
  (*      [------]                          *) tc (z a b) (z m a) (z a a) (z m b);
  (*      [----------]                      *) tc (z a b) (z m p) (z a p) (z m b);
  (*      [------------------]              *) tc (z a b) (z m b) (z a b) (z m b);
  (*      [----------------------]          *) tc (z a b) (z m n) (z a b) (z m n);
  (*      [-------------------------------  *) tc (z a b) (z m j) (z a b) (z m j);
  (*             |                          *) tc (z a b) (z a a) (z a a) (z a b);
  (*             [---]                      *) tc (z a b) (z a p) (z a p) (z a b);
  (*             [-----------]              *) tc (z a b) (z a b) (z a b) (z a b);
  (*             [---------------]          *) tc (z a b) (z a n) (z a b) (z a n);
  (*             [------------------------  *) tc (z a b) (z a j) (z a b) (z a j);
  (*                 |                      *) tc (z a b) (z p p) (z p p) (z a b);
  (*                 [---]                  *) tc (z a b) (z p q) (z p q) (z a b);
  (*                 [-------]              *) tc (z a b) (z p b) (z p b) (z a b);
  (*                 [-----------]          *) tc (z a b) (z p n) (z p b) (z a n);
  (*                 [--------------------  *) tc (z a b) (z p j) (z p b) (z a j);
  (*                         |              *) tc (z a b) (z b b) (z b b) (z a b);
  (*                         [---]          *) tc (z a b) (z b n) (z b b) (z a n);
  (*                         [------------  *) tc (z a b) (z b j) (z b b) (z a j);
  (*                             |          *) tc (z a b) (z n n)   bot   (z a n);
  (*                             [---]      *) tc (z a b) (z n n')  bot   (z a n');
  (*                             [--------  *) tc (z a b) (z n j)   bot   (z a j);

  (* R = [p;+oo] *)
  (*  -oo    m    a   p        b   n   +oo  *)
  (*                R [-------------------  *)
  (*                                        *)
  (*                                        *) tc (z p j)   bot     bot   (z p j);
  (*  -------]                              *) tc (z p j) (z i m)   bot   (z i j);
  (*  ----------------]                     *) tc (z p j) (z i p) (z p p) (z i j);
  (*  -------------------------]            *) tc (z p j) (z i b) (z p b) (z i j);
  (*  ------------------------------------  *) tc (z p j) (z i j) (z p j) (z i j);
  (*         |                              *) tc (z p j) (z m m)   bot   (z m j);
  (*         [----]                         *) tc (z p j) (z m a)   bot   (z m j);
  (*         [--------]                     *) tc (z p j) (z m p) (z p p) (z m j);
  (*         [-----------------]            *) tc (z p j) (z m b) (z p b) (z m j);
  (*         [----------------------------  *) tc (z p j) (z m j) (z p j) (z m j);
  (*                  |                     *) tc (z p j) (z p p) (z p p) (z p j);
  (*                  [--------]            *) tc (z p j) (z p b) (z p b) (z p j);
  (*                  [-------------------  *) tc (z p j) (z p j) (z p j) (z p j);
  (*                           |            *) tc (z p j) (z b b) (z b b) (z p j);
  (*                           [---]        *) tc (z p j) (z b n) (z b n) (z p j);
  (*                           [----------  *) tc (z p j) (z b j) (z b j) (z p j);

  test_end ()
