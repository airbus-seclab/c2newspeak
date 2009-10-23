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

type t = (int * int) option

let from_bounds a b =
  assert (a <= b);
  Some (a, b)

let top = Some (min_int, max_int)

let bottom = None

let (<=%) a b = match (a, b) with
  | None       , _           -> true
  | Some _     , None        -> false
  | Some (a, b), Some (c, d) -> c <= a && b <= d

let join a b = match (a, b) with
  | None, _ -> b
  | _, None -> a
  | Some (l1, u1), Some (l2, u2) ->
      Some (min l1 l2, max u1 u2)

let meet a b = match (a, b) with
  | None, _ -> None
  | _, None -> None
  | Some (l1, u1), Some (l2, u2) ->
      begin
        if l2 <= l1 then
          begin
            if u2 < l1
              then None
              else from_bounds l1 (min u1 u2)
          end
        else
          begin
            if u1 < l2
              then None
              else from_bounds l2 (min u1 u2)
          end
      end

let widen a b =
  match (a, b) with
  | None         , _             -> None
  | _            , None          -> None
  | Some (l1, u1), Some (l2, u2) -> let l = if l2 < l1
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

let plus ab cd = match (ab, cd) with
  | None       , _           -> None
  | _          , None        -> None
  | Some (a, b), Some (c, d) -> Some (add_overflow a c, add_overflow b d)

let neg = function
  | None -> None
  | Some (a, b) -> Some (- b, - a)

let to_string =
  let string_of_inf x =
    if      x = max_int then "+oo"
    else if x = min_int then "-oo"
    else string_of_int x
  in function
  | None        -> "(bot)"
  | Some (a, b) when a = min_int && b = max_int -> "(top)"
  | Some (a, b) when a = b -> "{" ^ string_of_int a ^ "}"
  | Some (a, b)            -> "[" ^ string_of_inf a ^ ";"
                                  ^ string_of_inf b ^ "]"

(* safe mult *)
let smul a b =
  if      a = 0       then 0
  else if b = 0       then 0
  else if a = max_int then
    if b > 0
      then max_int
      else min_int
  else if a = min_int then
    if a > 0
      then min_int
      else max_int
  else if b = max_int then
    if a > 0
      then max_int
      else min_int
  else if b = min_int then
    if a > 0
      then min_int
      else max_int
  else a * b


let mult_pp r1 r2 = match (r1, r2) with
  | None       , _    -> None
  | _          , None -> None
  | Some (a, b), Some (c,d) -> from_bounds (smul a c) (smul b d)

let mult_mp r1 r2 = match (r1, r2) with
  | None       , _    -> None
  | _          , None -> None
  | Some (a, b), Some (c,d) -> from_bounds (smul a d) (smul b c)

let mult_mm r1 r2 = match (r1, r2) with
  | None       , _    -> None
  | _          , None -> None
  | Some (a, b), Some (c,d) -> from_bounds (smul b d) (smul a c)

let mult r1 r2 =
  let r1p = meet r1 (Some (0, max_int)) in
  let r1m = meet r1 (Some (min_int, 0)) in
  let r2p = meet r2 (Some (0, max_int)) in
  let r2m = meet r2 (Some (min_int, 0)) in
  List.fold_left join bottom [ mult_pp r1p r2p
                             ; mult_mp r1m r2p
                             ; mult_mp r2m r1p 
                             ; mult_mm r1m r2m
                             ]

let eval lookup x =
  let bool_top   = from_bounds 0 1 in
  let bool_true  = from_bounds 1 1 in
  let bool_false = from_bounds 0 0 in
  let rec eval = function 
  | Prog.Const n            -> from_bounds n n
  | Prog.Var v'             -> lookup v'
  | Prog.Op (Prog.Plus,  e1, e2) -> plus (eval e1) (eval e2)
  | Prog.Op (Prog.Minus, e1, e2) -> plus (eval e1) (neg (eval e2))
  | Prog.Not e' -> begin match eval e' with
                           | Some (0,0)            -> bool_true
                           | Some (a,_) when a > 0 -> bool_false
                           | Some (_,b) when b < 0 -> bool_false
                           | _                     -> bool_top
                   end
  | Prog.Op (Prog.Eq, e1, e2) ->
                           let r1 = eval e1 in
                           let r2 = eval e2 in
                           if meet r1 r2 = None then bool_false
                           else begin match (r1, r2) with
                           | Some (a, b), Some (c, d) when a = b
                                                       &&  b = c
                                                       &&  c = d -> bool_true
                           | _ -> bool_top
                           end
  | Prog.Op (Prog.Mult, e1, e2) -> mult (eval e1) (eval e2)
  | _ -> failwith "not impl"
  in
  eval x

let dom = { Domain.top       = top
          ; Domain.bottom    = bottom
          ; Domain.from_val  = (fun n -> from_bounds n n)
          ; Domain.incl      = (<=%)
          ; Domain.join      = join
          ; Domain.meet      = meet
          ; Domain.to_string = to_string
          }
