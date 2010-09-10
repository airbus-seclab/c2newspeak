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

open Utils.Maybe
module Maybe_monad = Utils.Monad(Utils.Maybe)
open Maybe_monad

type 'a t = ('a * 'a) option

let from_bounds a b =
  assert (a <= b);
  Some (a, b)

let empty = None

let (<=%) r1 r2 =
  match (r1, r2) with
    | None, _ -> true
    | Some _, None -> false
    | Some (a, b), Some (c, d) ->
        c <= a && b <= d

let join r1 r2 =
  match (r1, r2) with
    | None, _ -> r2
    | Some _, None -> r1
    | Some (l1, u1), Some (l2, u2) -> Some (min l1 l2, max u1 u2)
            
let meet x1 x2 =
  x1 >>= fun (l1, u1) ->
  x2 >>= fun (l2, u2) ->
    if (l2 > u1 || l1 > u2) then
      empty
    else
      from_bounds (max l1 l2) (min u1 u2)

let widen x1 x2 =
  x1 >>= fun (l1, u1) ->
  x2 >>= fun (l2, u2) ->
    let l = if l2 < l1
      then min_int
      else l1
    in
    let u = if u1 < u2
      then max_int
      else u1
    in
    Some (l, u)

let is_infinite x =
  x == max_int || x == min_int

let add_overflow n x =
  if (is_infinite x) then x
  else x + n


let plus =
  liftM2 (fun (l1, u1) (l2, u2) ->
    (add_overflow l1 l2, add_overflow u1 u2)
  )

let neg r =
  r >>= (fun (a, b) -> Some (- b, - a))

let to_string =
  let string_of_inf x =
    if      x = max_int then "+oo"
    else if x = min_int then "-oo"
    else string_of_int x
  in
  maybe "(bot)"
    ( function
      | (a, b) when a = min_int && b = max_int -> "(top)"
      | (a, b) when a = b -> "{" ^ string_of_int a ^ "}"
      | (a, b)            -> "[" ^ string_of_inf a ^ ";"
                                 ^ string_of_inf b ^ "]"
    )

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

let mult_pp = liftM2 (fun (a, b) (c,d) -> (smul a c, smul b d))
let mult_mp = liftM2 (fun (a, b) (c,d) -> (smul a d, smul b c))
let mult_mm = liftM2 (fun (a, b) (c,d) -> (smul b d, smul a c))

let mult r1 r2 =
  let r1p = meet r1 (Some (0, max_int)) in
  let r1m = meet r1 (Some (min_int, 0)) in
  let r2p = meet r2 (Some (0, max_int)) in
  let r2m = meet r2 (Some (min_int, 0)) in
  List.fold_left join None [ mult_pp r1p r2p
                           ; mult_mp r1m r2p
                           ; mult_mp r2m r1p
                           ; mult_mm r1m r2m
                           ]
