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

type 'a t =
  | Interval of 'a * 'a
  | Empty

let to_string =
  let p_int () x =
    if      x = max_int then "+oo"
    else if x = min_int then "-oo"
    else string_of_int x
  in
  function
    | Empty -> "(bot)"
    | Interval (a, b) when a = min_int && b = max_int -> "(top)"
    | Interval (a, b) when a = b -> Printf.sprintf "{%d}" a
    | Interval (a, b)            -> Printf.sprintf "[%a;%a]" p_int a p_int b

let from_bounds a b =
  assert (a <= b);
  Interval (a, b)

let with_size n =
  from_bounds 0 (n-1)

let top_int = from_bounds min_int max_int

let bottom_int = Empty

let (<=%) r1 r2 =
  match (r1, r2) with
    | Empty, _ -> true
    | Interval _, Empty -> false
    | Interval (a, b), Interval (c, d) ->
        c <= a && b <= d

let join r1 r2 =
  match (r1, r2) with
    | Empty, _ -> r2
    | Interval _, Empty -> r1
    | Interval (l1, u1), Interval (l2, u2) -> from_bounds (min l1 l2) (max u1 u2)
            
let meet x1 x2 = match (x1, x2) with
  | Interval (l1, u1), Interval (l2, u2) ->
      if (l2 > u1 || l1 > u2) then
        Empty
      else
        from_bounds (max l1 l2) (min u1 u2)
  | Empty, _ | _, Empty -> Empty

let widen x1 x2 = match (x1, x2) with
  | Interval (l1, u1), Interval (l2, u2) ->
      let l = if l2 < l1
        then min_int
        else l1
      in
      let u = if u1 < u2
        then max_int
        else u1
      in
      from_bounds l u
  | Empty, _ | _, Empty -> Empty

let is_infinite x =
  x == max_int || x == min_int

let add_overflow x y =
  match (is_infinite x,is_infinite y) with
    | false, false -> x + y
    | false, true -> y
    | true, false -> x
    | true, true -> assert false

let plus x1 x2 = match (x1, x2) with
  | Interval (l1, u1), Interval (l2, u2) ->
      let x = add_overflow l1 l2 in
      let y = add_overflow u1 u2 in
      from_bounds x y
  | Empty, _ | _, Empty -> Empty

let neg_overflow n =
  if      n = min_int then max_int
  else if n = max_int then min_int
  else - n

let neg = function
  | Empty -> Empty
  | Interval (a, b) -> from_bounds (neg_overflow b) (neg_overflow a)

(* safe mult *)

let smul a b =
  let inf_of_sign c = if c then max_int else min_int in
  if      a = 0       then 0
  else if b = 0       then 0
  else if a = max_int then inf_of_sign (b > 0)
  else if a = min_int then inf_of_sign (b < 0)
  else if b = max_int then inf_of_sign (a > 0)
  else if b = min_int then inf_of_sign (a < 0)
  else a * b

let mult_pp = function
  | Interval (a, b), Interval (c,d) -> Interval (smul a c, smul b d)
  | Empty, _ | _, Empty -> Empty

let mult_mp = function
  | Interval (a, b), Interval (c,d) -> Interval (smul a d, smul b c)
  | Empty, _ | _, Empty -> Empty

let mult_mm = function
  | Interval (a, b), Interval (c,d) -> Interval (smul b d, smul a c)
  | Empty, _ | _, Empty -> Empty

let mult r1 r2 =
  let r1p = meet r1 (from_bounds 0  max_int) in
  let r1m = meet r1 (from_bounds min_int  0) in
  let r2p = meet r2 (from_bounds 0  max_int) in
  let r2m = meet r2 (from_bounds min_int  0) in
  List.fold_left join Empty [ mult_pp (r1p, r2p)
                            ; mult_mp (r1m, r2p)
                            ; mult_mp (r2m, r1p)
                            ; mult_mm (r1m, r2m)
                            ]
