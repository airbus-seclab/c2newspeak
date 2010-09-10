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

  Copyright 2009, 2010 Etienne Millon <etienne.millon@eads.net>

 *)

(** @author Etienne Millon <etienne.millon@eads.net> *)
open Domain
open Interval
open Tap

module Test_range = struct
open Range

let run _ =
  test_plan 213;

  (* Incl + Join + Meet *)

  let bot = dom.bottom in
  let tc r q m j =
    let name_m = dom.to_string r ^" /\\ "
               ^ dom.to_string q ^" == "
               ^ dom.to_string m in
    let name_j = dom.to_string r ^" \\/ "
               ^ dom.to_string q ^" == "
               ^ dom.to_string j in
    assert_equal ~printer:dom.to_string (dom.meet r q) m name_m;
    assert_equal ~printer:dom.to_string (dom.join r q) j name_j;
  in

  let i  = min_int in let m  = (-22)   in let m' = (-20)   in
  let a  = (-8)    in let p  = 2       in let q  = 7       in
  let b  = 55      in let n  = 123     in let n' = 1503    in
  let j  = max_int in

  let z = from_bounds in

  let t = tc bot in          (* R = {} *)
                             (*  -oo          a         b         +oo  *)
  t   bot     bot     bot;   (*                                        *)
  t (z i a)   bot   (z i a); (*  -------------]                        *)
  t (z i j)   bot   (z i j); (*  ------------------------------------  *)
  t (z a a)   bot   (z a a); (*               |                        *)
  t (z a b)   bot   (z a b); (*               [---------]              *)
  t (z a j)   bot   (z a j); (*               [----------------------  *)

  let t = tc (z i p) in      (* R = [-oo;p] *)
                             (*  -oo    m    a   p        b   n   +oo  *)
                             (*  ----------------] R                   *)
                             (*                                        *)
  t   bot     bot   (z i p); (*                                        *)
  t (z i m) (z i m) (z i p); (*  -------]                              *)
  t (z i p) (z i p) (z i p); (*  ----------------]                     *)
  t (z i b) (z i p) (z i b); (*  -------------------------]            *)
  t (z i j) (z i p) (z i j); (*  ------------------------------------  *)
  t (z m m) (z m m) (z i p); (*         |                              *)
  t (z m a) (z m a) (z i p); (*         [----]                         *)
  t (z m p) (z m p) (z i p); (*         [--------]                     *)
  t (z m b) (z m p) (z i b); (*         [-----------------]            *)
  t (z m j) (z m p) (z i j); (*         [----------------------------  *)
  t (z p p) (z p p) (z i p); (*                  |                     *)
  t (z p b) (z p p) (z i b); (*                  [--------]            *)
  t (z p j) (z p p) (z i j); (*                  [-------------------  *)
  t (z b b)   bot   (z i b); (*                           |            *)
  t (z b n)   bot   (z i n); (*                           [---]        *)
  t (z b j)   bot   (z i j); (*                           [----------  *)

  let t = tc (z i j) in      (* R = [-oo;+oo] *)
                             (*  -oo          a         b         +oo  *)
  t (z i j) (z i j) (z i j); (*                                        *)
  t (z i a) (z i a) (z i j); (*  -------------]                        *)
  t (z i j) (z i j) (z i j); (*  ------------------------------------  *)
  t (z a a) (z a a) (z i j); (*               |                        *)
  t (z a b) (z a b) (z i j); (*               [---------]              *)
  t (z a j) (z a j) (z i j); (*               [----------------------  *)

  let t = tc (z p p) in      (* R = {p} *)
                             (*  -oo   m    a     p     b    n    +oo  *)
                             (*                   | R                  *)
                             (*                                        *)
  t   bot     bot   (z p p); (*                                        *)
  t (z i a)   bot   (z i p); (*  -----------]                          *)
  t (z i p) (z p p) (z i p); (*  -----------------]                    *)
  t (z i b) (z p p) (z i b); (*  -----------------------]              *)
  t (z i j) (z p p) (z i j); (*  ------------------------------------  *)
  t (z m m)   bot   (z m p); (*        |                               *)
  t (z m a)   bot   (z m p); (*        [----]                          *)
  t (z m p) (z p p) (z m p); (*        [----------]                    *)
  t (z m b) (z p p) (z m b); (*        [----------------]              *)
  t (z m j) (z p p) (z m j); (*        [-----------------------------  *)
  t (z p p) (z p p) (z p p); (*                   |                    *)
  t (z p b) (z p p) (z p b); (*                   [-----]              *)
  t (z p j) (z p p) (z p j); (*                   [------------------  *)
  t (z b b)   bot   (z p b); (*                         |              *)
  t (z b n)   bot   (z p n); (*                         [----]         *)
  t (z b j)   bot   (z p j); (*                         [------------  *)

  let t = tc (z a b) in      (* R = [a;b] *)
                             (* -oo  m  m'  a   p   q   b   n   n' +oo *)
                             (*             [-----R-----]              *)
                             (*                                        *)
  t   bot     bot   (z a b); (*                                        *)
  t (z i m)   bot   (z i b); (* -----]                                 *)
  t (z i a) (z a a) (z i b); (* ------------]                          *)
  t (z i p) (z a p) (z i b); (* ----------------]                      *)
  t (z i b) (z a b) (z i b); (* ------------------------]              *)
  t (z i n) (z a b) (z i n); (* ----------------------------]          *)
  t (z i j) (z a b) (z i j); (* -------------------------------------  *)
  t (z m m)   bot   (z m b); (*      |                                 *)
  t (z m m')  bot   (z m b); (*      [--]                              *)
  t (z m a) (z a a) (z m b); (*      [------]                          *)
  t (z m p) (z a p) (z m b); (*      [----------]                      *)
  t (z m b) (z a b) (z m b); (*      [------------------]              *)
  t (z m n) (z a b) (z m n); (*      [----------------------]          *)
  t (z m j) (z a b) (z m j); (*      [-------------------------------  *)
  t (z a a) (z a a) (z a b); (*             |                          *)
  t (z a p) (z a p) (z a b); (*             [---]                      *)
  t (z a b) (z a b) (z a b); (*             [-----------]              *)
  t (z a n) (z a b) (z a n); (*             [---------------]          *)
  t (z a j) (z a b) (z a j); (*             [------------------------  *)
  t (z p p) (z p p) (z a b); (*                 |                      *)
  t (z p q) (z p q) (z a b); (*                 [---]                  *)
  t (z p b) (z p b) (z a b); (*                 [-------]              *)
  t (z p n) (z p b) (z a n); (*                 [-----------]          *)
  t (z p j) (z p b) (z a j); (*                 [--------------------  *)
  t (z b b) (z b b) (z a b); (*                         |              *)
  t (z b n) (z b b) (z a n); (*                         [---]          *)
  t (z b j) (z b b) (z a j); (*                         [------------  *)
  t (z n n)   bot   (z a n); (*                             |          *)
  t (z n n')  bot   (z a n');(*                             [---]      *)
  t (z n j)   bot   (z a j); (*                             [--------  *)

  let t = tc (z p j) in      (* R = [p;+oo] *)
                             (*  -oo    m    a   p        b   n   +oo  *)
                             (*                R [-------------------  *)
                             (*                                        *)
  t   bot     bot   (z p j); (*                                        *)
  t (z i m)   bot   (z i j); (*  -------]                              *)
  t (z i p) (z p p) (z i j); (*  ----------------]                     *)
  t (z i b) (z p b) (z i j); (*  -------------------------]            *)
  t (z i j) (z p j) (z i j); (*  ------------------------------------  *)
  t (z m m)   bot   (z m j); (*         |                              *)
  t (z m a)   bot   (z m j); (*         [----]                         *)
  t (z m p) (z p p) (z m j); (*         [--------]                     *)
  t (z m b) (z p b) (z m j); (*         [-----------------]            *)
  t (z m j) (z p j) (z m j); (*         [----------------------------  *)
  t (z p p) (z p p) (z p j); (*                  |                     *)
  t (z p b) (z p b) (z p j); (*                  [--------]            *)
  t (z p j) (z p j) (z p j); (*                  [-------------------  *)
  t (z b b) (z b b) (z p j); (*                           |            *)
  t (z b n) (z b n) (z p j); (*                           [---]        *)
  t (z b j) (z b j) (z p j); (*                           [----------  *)

  (* Widen *)

  let assert_widen a b w =
    let n = dom.to_string a ^ " W "
          ^ dom.to_string b ^ " = "
          ^ dom.to_string w in
    assert_equal ~printer:dom.to_string (dom.widen a b) w n
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

  (* assert (not [a;b] == [c;d]) *)
  let tc_not a b c d name =
    let (r, _) =
      dom.eval
        (function Prog.G "v" -> from_bounds a b | _ -> invalid_arg "no variable")
        (function Prog.G "v" -> Prog.Heap "v"   | _ -> invalid_arg "no variable")
        (Prog.Not (Prog.Lval (Prog.G "v", Prog.Int)))
    in
    assert_equal ~printer:dom.to_string r (from_bounds c d) name
  in

  tc_not 0       0       1 1 "not [0;0] == [1;1]";
  tc_not 1       1       0 0 "not [1;1] == [0;0]";
  tc_not min_int max_int 0 1 "not  top  == [0;1]";
  tc_not min_int (-1)    0 0 "not [-oo;-1] == [0;0]";
  tc_not 1       max_int 0 0 "not [1;+oo] == [0;0]";

  (* assert [a;b] .==. [c;d] == [e;f] *)
  let tc_eqeq a b c d e f name =
    let (r, _) = dom.eval (function
        | Prog.G "x" -> from_bounds a b
        | Prog.G "y" -> from_bounds c d
        | _ -> invalid_arg "no variable"
      ) ( function
        | Prog.G "x" -> Prog.Heap "x"
        | Prog.G "y" -> Prog.Heap "y"
        | _ -> invalid_arg "no variable"
      )
      (Prog.Op (Prog.Eq, Prog.Lval (Prog.G "x", Prog.Int),
                           Prog.Lval (Prog.G "y", Prog.Int)))
    in
    assert_equal ~printer:dom.to_string r (from_bounds e f) name
  in

  tc_eqeq 2 5  8  13 0 0 "[2;5] .==. [8;13] == [0;0]";
  tc_eqeq 3 12 10 16 0 1 "[3;12] .==. [10;16] == [0;1]";
  tc_eqeq 5 5  5  5  1 1 "{5} .==. {5} == [1;1]";
  tc_eqeq 0 0  5  5  0 0 "{0} .==. {5} == [0;0]";

  assert_equal ~printer:dom.to_string (fst (dom.eval
  (function Prog.G "v" -> from_bounds 3 5 | _ -> invalid_arg "no variable")
  (function Prog.G "v" -> Prog.Heap "v"   | _ -> invalid_arg "no variable")
  (Prog.Op (Prog.Minus, Prog.Const (Prog.CInt 0), Prog.Lval (Prog.G "v", Prog.Int)))))
    (from_bounds (-5) (-3)) "- [3;5] == [-5;-3]";

  (* assert [a;b] .*. [c;d] == [e;f] *)
  let tc_mult a b c d e f name =
    assert_equal ~printer:dom.to_string (fst (dom.eval (function
      | Prog.G "x" -> from_bounds a b
      | Prog.G "y" -> from_bounds c d
      | _ -> invalid_arg "no variable"
    ) ( function
      | Prog.G "x" -> Prog.Heap "x"
      | Prog.G "y" -> Prog.Heap "y"
      | _ -> invalid_arg "no variable"
    ) (Prog.Op (Prog.Mult, Prog.Lval (Prog.G "x", Prog.Int),
                           Prog.Lval (Prog.G "y", Prog.Int))))) (from_bounds e f) name
  in

  tc_mult   5    5    2    8    10   40  "[5;5] .*. [2;8] == [10;40]";
  tc_mult (-2) (-1)   5   12  (-24) (-5) "[-2;-1] .*. [5;12] == [-24;-5]";
  tc_mult   5   12  (-2) (-1) (-24) (-5) "[5;12] .*. [-2;-1] == [-24;-5]";
  tc_mult   0    2    0    3     0    6  "[0;2] .*. [0;3] == [0;6]";
  tc_mult (-8) (-7) (-2) (-1)    7   16  "[-8;-7] .*. [-2;-1] == [7;16]";
  tc_mult (-5)   1  (-3)   8  (-40)  15  "[-5;1] .*. [-3;8] == [-40;15]";
  tc_mult   2    j    2    j     4    j  "[2;+oo] .*. [2;+oo] == [4;+oo]";
  tc_mult   i  (-1)   i  (-1)    1    j  "[-oo:-1] .*. [-oo;-1] == [1;+oo]";
  tc_mult   i    0    1    1     i    0  "{1} x [-oo;0] == [-oo;0]";

  test_end ()
end

module Test_box = struct
open Box
let run _ =
  test_plan 11;

  let dom = Range.dom in

  let get_gvar v x = environment x (Prog.G v) in
  let get_lvar n x = environment x (Prog.L n) in
  assert_exn (fun _ -> ignore (get_gvar "x" bottom)) "Box.bottom has no variables";

  assert_equal (get_gvar "x" (top dom)) Range.dom.top
                        "Box.top returns top for global variables";

  let ae got exp name =
    assert_equal ~cmp:Box.equal ~printer:to_string got exp name
  in

  let sg var v =
    singleton ~typ:(Prog.Int) dom (Prog.G var) (const Range.dom v)
  in
  let intvl var x y =
    singleton ~typ:(Prog.Int) dom (Prog.G var) (from_bounds x y)
  in

  let i2 = sg "i" 2 in
  let j5 = sg "j" 5 in

  let i2j5 = meet i2 j5 in
  assert_equal ~printer:Range.dom.to_string (get_gvar "i" i2j5)
      (const Range.dom 2) "{i: 2} /\\ {j: 5} . i = {2}";
  assert_equal ~printer:Range.dom.to_string (get_gvar "j" i2j5)
      (const Range.dom 5) "{i: 2} /\\ {j: 5} . i = {5}";

  ae (join i2 j5) i2j5 "{i: 2} \\/ {j: 5} = { i: 2, j: 5 }";

  ae (meet (sg "i" 3) i2j5) bottom
     "{i: 2, j: 5} / i <= {3} = {j: 5}";

  ae (join (join (intvl "i" min_int 0) (sg "j" 5))
           (join (intvl "i" 0 max_int) (sg "j" 8))
     )
     (join (intvl "i" min_int max_int) (intvl "j" 5 8))
     "{i: R-, j: 5} \\/ {i: R+, j: 8} = {j: [5;8]}"
     ;

  ae (meet (intvl "i" min_int 0) i2) bottom "{i: 2} /\\ {i: R-} = bot";

  let b = Box.set_var (Prog.G "x") (from_bounds 5 5) (top dom) in
  assert_equal ~printer:Range.dom.to_string (get_gvar "x" b) (from_bounds 5 5)
      "Global <- 5; Global == 5";

  (* local variables *)

  assert_equal (get_lvar 0 (top dom)) Range.dom.top
                        "Box.top returns top for local variables";

  let b = Box.set_var (Prog.L 0) (from_bounds 5 5) (top dom) in
  assert_equal ~printer:Range.dom.to_string (get_lvar 0 b) (from_bounds 5 5)
      "Local <- 5; Local == 5";

  test_end ()
end

module Test_const = struct
open Const

let run _ =
  test_plan (16 * 3);
  let top = dom.top in
  let bot = dom.bottom in
  let c = const dom in

  let tc a b r j m =
    let n_op ?r op = dom.to_string a ^ " " ^ op ^ " " ^ dom.to_string b
      ^ (match r with None -> "" | Some x -> " = "^ dom.to_string x)
    in
    let name_inc = n_op (if r then "<=" else "</") in
    let name_j = n_op ~r:j "\\/" in
    let name_m = n_op ~r:m "/\\" in
    assert_equal (dom.join a b = b) r name_inc;
    assert_equal ~printer:dom.to_string (dom.join a b) j name_j;
    assert_equal ~printer:dom.to_string (dom.meet a b) m name_m;
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
