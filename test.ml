open Tap
open Range

let assert_incl a b =
  assert_true (a <=% b)

let assert_not_incl a b =
  assert_false (a <=% b)

let range _ =
  test_plan 284;

  (* Incl + Join + Meet *)

  let bot = bottom in
  let tc r q i m j =
    let name_i = to_string r ^ (if i then " <= "
                                     else " </= ")
               ^ to_string q in
    let name_m = to_string r ^" /\\ "
               ^ to_string q ^" == "
               ^ to_string m in
    let name_j = to_string r ^" \\/ "
               ^ to_string q ^" == "
               ^ to_string j in
    if i then assert_incl     r q name_i
         else assert_not_incl r q name_i;
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
  (*                                        *) tc   bot     bot   true    bot     bot;
  (*  -------------]                        *) tc   bot   (z i a) true    bot   (z i a);
  (*  ------------------------------------  *) tc   bot   (z i j) true    bot   (z i j);
  (*               |                        *) tc   bot   (z a a) true    bot   (z a a);
  (*               [---------]              *) tc   bot   (z a b) true    bot   (z a b);
  (*               [----------------------  *) tc   bot   (z a j) true    bot   (z a j);
                                                                        
  (* R = [-oo;p] *)                                                     
  (*  -oo    m    a   p        b   n   +oo  *)                          
  (*  ----------------] R                   *)                          
  (*                                        *)                          
  (*                                        *) tc (z i p)   bot   false   bot   (z i p);
  (*  -------]                              *) tc (z i p) (z i m) false (z i m) (z i p);
  (*  ----------------]                     *) tc (z i p) (z i p) true  (z i p) (z i p);
  (*  -------------------------]            *) tc (z i p) (z i b) true  (z i p) (z i b);
  (*  ------------------------------------  *) tc (z i p) (z i j) true  (z i p) (z i j);
  (*         |                              *) tc (z i p) (z m m) false (z m m) (z i p);
  (*         [----]                         *) tc (z i p) (z m a) false (z m a) (z i p);
  (*         [--------]                     *) tc (z i p) (z m p) false (z m p) (z i p);
  (*         [-----------------]            *) tc (z i p) (z m b) false (z m p) (z i b);
  (*         [----------------------------  *) tc (z i p) (z m j) false (z m p) (z i j);
  (*                  |                     *) tc (z i p) (z p p) false (z p p) (z i p);
  (*                  [--------]            *) tc (z i p) (z p b) false (z p p) (z i b);
  (*                  [-------------------  *) tc (z i p) (z p j) false (z p p) (z i j);
  (*                           |            *) tc (z i p) (z b b) false   bot   (z i b);
  (*                           [---]        *) tc (z i p) (z b n) false   bot   (z i n);
  (*                           [----------  *) tc (z i p) (z b j) false   bot   (z i j);
                                                                        
  (* R = [-oo;+oo] *)                                                   
  (*  -oo          a         b         +oo  *)                          
  (*                                        *) tc   top     top   true    top     top;
  (*  -------------]                        *) tc   top   (z i a) false (z i a)   top;
  (*  ------------------------------------  *) tc   top   (z i j) true  (z i j)   top;
  (*               |                        *) tc   top   (z a a) false (z a a)   top;
  (*               [---------]              *) tc   top   (z a b) false (z a b)   top;
  (*               [----------------------  *) tc   top   (z a j) false (z a j)   top;
                                                                        
                                                                        
  (* R = {p} *)                                                         
  (*  -oo   m    a     p     b    n    +oo  *)                          
  (*                   | R                  *)                          
  (*                                        *)                          
  (*                                        *) tc (z p p)   bot   false   bot   (z p p);
  (*  -----------]                          *) tc (z p p) (z i a) false   bot   (z i p);
  (*  -----------------]                    *) tc (z p p) (z i p) true  (z p p) (z i p);
  (*  -----------------------]              *) tc (z p p) (z i b) true  (z p p) (z i b);
  (*  ------------------------------------  *) tc (z p p) (z i j) true  (z p p) (z i j);
  (*        |                               *) tc (z p p) (z m m) false   bot   (z m p);
  (*        [----]                          *) tc (z p p) (z m a) false   bot   (z m p);
  (*        [----------]                    *) tc (z p p) (z m p) true  (z p p) (z m p);
  (*        [----------------]              *) tc (z p p) (z m b) true  (z p p) (z m b);
  (*        [-----------------------------  *) tc (z p p) (z m j) true  (z p p) (z m j);
  (*                   |                    *) tc (z p p) (z p p) true  (z p p) (z p p);
  (*                   [-----]              *) tc (z p p) (z p b) true  (z p p) (z p b);
  (*                   [------------------  *) tc (z p p) (z p j) true  (z p p) (z p j);
  (*                         |              *) tc (z p p) (z b b) false   bot   (z p b);
  (*                         [----]         *) tc (z p p) (z b n) false   bot   (z p n);
  (*                         [------------  *) tc (z p p) (z b j) false   bot   (z p j);
                                                                        
  (* R = [a;b] *)                                                       
  (* -oo  m  m'  a   p   q   b   n   n' +oo *)                          
  (*             [-----R-----]              *)                          
  (*                                        *)                          
  (*                                        *) tc (z a b)   bot   false   bot   (z a b);
  (* -----]                                 *) tc (z a b) (z i m) false   bot   (z i b);
  (* ------------]                          *) tc (z a b) (z i a) false (z a a) (z i b);
  (* ----------------]                      *) tc (z a b) (z i p) false (z a p) (z i b);
  (* ------------------------]              *) tc (z a b) (z i b) true  (z a b) (z i b);
  (* ----------------------------]          *) tc (z a b) (z i n) true  (z a b) (z i n);
  (* -------------------------------------  *) tc (z a b) (z i j) true  (z a b) (z i j);
  (*      |                                 *) tc (z a b) (z m m) false   bot   (z m b);
  (*      [--]                              *) tc (z a b) (z m m')false   bot   (z m b);
  (*      [------]                          *) tc (z a b) (z m a) false (z a a) (z m b);
  (*      [----------]                      *) tc (z a b) (z m p) false (z a p) (z m b);
  (*      [------------------]              *) tc (z a b) (z m b) true  (z a b) (z m b);
  (*      [----------------------]          *) tc (z a b) (z m n) true  (z a b) (z m n);
  (*      [-------------------------------  *) tc (z a b) (z m j) true  (z a b) (z m j);
  (*             |                          *) tc (z a b) (z a a) false (z a a) (z a b);
  (*             [---]                      *) tc (z a b) (z a p) false (z a p) (z a b);
  (*             [-----------]              *) tc (z a b) (z a b) true  (z a b) (z a b);
  (*             [---------------]          *) tc (z a b) (z a n) true  (z a b) (z a n);
  (*             [------------------------  *) tc (z a b) (z a j) true  (z a b) (z a j);
  (*                 |                      *) tc (z a b) (z p p) false (z p p) (z a b);
  (*                 [---]                  *) tc (z a b) (z p q) false (z p q) (z a b);
  (*                 [-------]              *) tc (z a b) (z p b) false (z p b) (z a b);
  (*                 [-----------]          *) tc (z a b) (z p n) false (z p b) (z a n);
  (*                 [--------------------  *) tc (z a b) (z p j) false (z p b) (z a j);
  (*                         |              *) tc (z a b) (z b b) false (z b b) (z a b);
  (*                         [---]          *) tc (z a b) (z b n) false (z b b) (z a n);
  (*                         [------------  *) tc (z a b) (z b j) false (z b b) (z a j);
  (*                             |          *) tc (z a b) (z n n) false   bot   (z a n);
  (*                             [---]      *) tc (z a b) (z n n')false   bot   (z a n');
  (*                             [--------  *) tc (z a b) (z n j) false   bot   (z a j);
                                                                        
  (* R = [p;+oo] *)                                                     
  (*  -oo    m    a   p        b   n   +oo  *)                          
  (*                R [-------------------  *)                          
  (*                                        *)                          
  (*                                        *) tc (z p j)   bot   false   bot   (z p j);
  (*  -------]                              *) tc (z p j) (z i m) false   bot   (z i j);
  (*  ----------------]                     *) tc (z p j) (z i p) false (z p p) (z i j);
  (*  -------------------------]            *) tc (z p j) (z i b) false (z p b) (z i j);
  (*  ------------------------------------  *) tc (z p j) (z i j) true  (z p j) (z i j);
  (*         |                              *) tc (z p j) (z m m) false   bot   (z m j);
  (*         [----]                         *) tc (z p j) (z m a) false   bot   (z m j);
  (*         [--------]                     *) tc (z p j) (z m p) false (z p p) (z m j);
  (*         [-----------------]            *) tc (z p j) (z m b) false (z p b) (z m j);
  (*         [----------------------------  *) tc (z p j) (z m j) true  (z p j) (z m j);
  (*                  |                     *) tc (z p j) (z p p) false (z p p) (z p j);
  (*                  [--------]            *) tc (z p j) (z p b) false (z p b) (z p j);
  (*                  [-------------------  *) tc (z p j) (z p j) true  (z p j) (z p j);
  (*                           |            *) tc (z p j) (z b b) false (z b b) (z p j);
  (*                           [---]        *) tc (z p j) (z b n) false (z b n) (z p j);
  (*                           [----------  *) tc (z p j) (z b j) false (z b j) (z p j);

  (* Widen *)

  let assert_widen a b w =
    let n = to_string a ^ " W "^ to_string b ^ " = " ^ to_string w in
    assert_equal ~printer:to_string (widen a b) w n
  in

  (* {} W x = x W {} = {} *)
  List.iter (fun r ->
    assert_widen bot r bot;
    assert_widen r bot bot;
  ) [bot; z a a; z a b; z i a; z a j; top];

  (* [a;b] W [p;+oo] = [a;+oo] *)
  assert_widen (z a b) (z p j) (z a j);

  (* [a;b] W [-oo;p] = [-oo;b] *)
  assert_widen (z a b) (z i p) (z i b);
  

  test_end ()
