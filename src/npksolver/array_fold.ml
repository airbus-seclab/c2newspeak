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

open Prog
open Domain

type 'a t = 'a

let to_string dom x =
  "folded (" ^ dom.to_string x ^ ")"

(* transform shift (x, _) to x *)
let rewrite_lv = function
  | Shift (lv, _) -> lv
  | lv -> lv

let rec rewrite = function
  | AddrOf lv -> AddrOf (rewrite_lv lv)
  | Const c -> Const c
  | Lval (lv, t) -> Lval (rewrite_lv lv, t)
  | Op (op, e1, e2) -> Op (op, rewrite e1, rewrite e2)
  | Not e -> Not (rewrite e)

let eval dom lookup e =
  dom.eval lookup (rewrite e)

let guard dom e =
  dom.guard (rewrite e)

let update dom text_lv ~old_value ~new_value =
  (rewrite_lv text_lv, dom.join old_value new_value)

let make dom =
  { dom with
    to_string = to_string dom
  ; eval      = eval dom
  ; guard     = guard dom
  ; update    = update dom
  }
