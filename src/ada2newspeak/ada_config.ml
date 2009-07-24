(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain

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

  Jasmine Duchon
  email : jasmine . duchon AT free . fr

*)

module Nat = Newspeak.Nat

let log2_sup n =
  let zero = Big_int.zero_big_int
  and one = Big_int.unit_big_int in
  let two = Big_int.succ_big_int one
  and eq_big_int a b = (Big_int.compare_big_int a b)=0
  in
  let rec aux n p =
    if eq_big_int n zero then one
    else
      if eq_big_int n one then p
      else aux (Big_int.add_big_int
                  (Big_int.div_big_int n two) (Big_int.mod_big_int n two))
        (Big_int.succ_big_int p)
  in Big_int.int_of_big_int (aux n zero)

let size_of_range inf sup =
  let (b_inf, b_sup) =
    if (Nat.compare inf sup)<=0 then (inf, sup)
    else (sup, inf)
  in
  let max = Big_int.max_big_int
    (Big_int.abs_big_int (Nat.to_big_int b_inf))
    (Big_int.abs_big_int (Big_int.succ_big_int (Nat.to_big_int b_sup)))
  in
  let min_bit = (log2_sup max) + 1
  in
    if min_bit <=8 then 8
    else if min_bit<=16 then 16
    else if min_bit<=32 then 32
    else if min_bit<=64 then 64
    else begin
      Npkcontext.report_error "Ada_utils.size_of_range"
        "type representation is too big"
    end

(* from the specification : 13.3.(49) *)
let size_of_boolean = 1

let size_of_char  = Config.size_of_char
let size_of_int   = Config.size_of_int
let size_of_float = Config.size_of_float

let integer_constraint =
  Syntax_ada.IntegerRangeConstraint(
    Newspeak.Nat.of_string("-2147483648"),
    Newspeak.Nat.of_string("2147483647"))
