(*
  This file is part of npksolver, a solver for Newspeak,
  a minimal language framework well suited for static analysis.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)
open Tap

module Test_range = struct
open Range

let assert_incl a b =
  assert_true (a <=% b)

let assert_not_incl a b =
  assert_false (a <=% b)

let run _ =
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

  let i  = min_int in let m  = (-22)   in let m' = (-20)   in
  let a  = (-8)    in let p  = 2       in let q  = 7       in
  let b  = 55      in let n  = 123     in let n' = 1503    in
  let j  = max_int in

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
  (*                                        *) tc (z i j) (z i j) true  (z i j) (z i j);
  (*  -------------]                        *) tc (z i j) (z i a) false (z i a) (z i j);
  (*  ------------------------------------  *) tc (z i j) (z i j) true  (z i j) (z i j);
  (*               |                        *) tc (z i j) (z a a) false (z a a) (z i j);
  (*               [---------]              *) tc (z i j) (z a b) false (z a b) (z i j);
  (*               [----------------------  *) tc (z i j) (z a j) false (z a j) (z i j);
                                                                        
                                                                        
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
  ) [bot; z a a; z a b; z i a; z a j; z i j];

  (* [a;b] W [p;+oo] = [a;+oo] *)
  assert_widen (z a b) (z p j) (z a j);

  (* [a;b] W [-oo;p] = [-oo;b] *)
  assert_widen (z a b) (z i p) (z i b);
  

  test_end ()
end

module Test_box = struct
open Box
let run _ =
  test_plan 7;
  assert_exn (fun s -> get_var s bottom) Not_found "x" "Box.bottom has no variables";

  let ae got exp name =
    assert_equal ~printer:to_string got exp name
  in

  let i2 = from_bounds "i" 2 2 in
  let j5 = from_bounds "j" 5 5 in

  let i2j5 = meet i2 j5 in
  assert_equal ~printer:Range.to_string (get_var "i" i2j5)
      (Range.from_bounds 2 2) "{i: 2} /\\ {j: 5} . i = {2}";
  assert_equal ~printer:Range.to_string (get_var "j" i2j5)
      (Range.from_bounds 5 5) "{i: 2} /\\ {j: 5} . i = {5}";

  ae (join i2 j5) i2j5 "{i: 2} \\/ {j: 5} . i = { }";

  ae (meet (from_bounds "i" 3 3) i2j5) bottom
     "{i: 2, j: 5} / i <= {3} = {j: 5}";

  ae (join (join (from_bounds "i" min_int 0) (from_bounds "j" 5 5))
           (join (from_bounds "i" 0 max_int) (from_bounds "j" 8 8))
     )
     (from_bounds "j" 5 8)
     "{i: R-, j: 5} \\/ {i: R+, j: 8} = {j: [5;8]}"
     ;

  ae (meet (from_bounds "i" min_int 0) i2) bottom "{i: 2} /\\ {i: R-} = bot";

  test_end ()
end

module Test_const = struct
open Const

let run _ =
  test_plan (16 * 3);
  let bot = bottom in
  let c = const in

  let tc a b r j m =
    let n_op ?r op = to_string a ^ " " ^ op ^ " " ^ to_string b
      ^ (match r with None -> "" | Some x -> " = "^to_string x)
    in
    let name_inc = n_op (if r then "<=" else "</") in
    let name_j = n_op ~r:j "\\/" in
    let name_m = n_op ~r:m "/\\" in
    assert_equal (a <=% b) r name_inc;
    assert_equal ~printer:to_string (join a b) j name_j;
    assert_equal ~printer:to_string (meet a b) m name_m;
  in

  tc  bot   bot  true   bot   bot;
  tc  bot  (c 2) true  (c 2)  bot;
  tc  bot  (c 5) true  (c 5)  bot;
  tc  bot   top  true   top   bot;
  tc (c 2)  bot  false (c 2)  bot;
  tc (c 2) (c 2) true  (c 2) (c 2);
  tc (c 2) (c 5) false  top   bot;
  tc (c 2)  top  true   top  (c 2);
  tc (c 5)  bot  false (c 5)  bot;
  tc (c 5) (c 2) false  top   bot;
  tc (c 5) (c 5) true  (c 5) (c 5);
  tc (c 5)  top  true   top  (c 5);
  tc  top   bot  false  top   bot;
  tc  top  (c 2) false  top  (c 2);
  tc  top  (c 5) false  top  (c 5);
  tc  top   top  true   top   top;

  test_end ()

end

let range = Test_range.run
let box   = Test_box.run
let const = Test_const.run