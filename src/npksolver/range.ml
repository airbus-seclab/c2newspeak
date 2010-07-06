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

open Utils.Lift

type t = (int * int) option

let from_bounds a b =
  assert (a <= b);
  Some (a, b)

let top = Some (min_int, max_int)

let bottom = None

let (<=%) r1 r2 =
  maybe true  (fun (a, b) ->
  maybe false (fun (c, d) ->
      c <= a && b <= d
  ) r2
  ) r1

let join r1 r2 =
  maybe r2 (fun (l1, u1) ->
  maybe r1 (fun (l2, u2) ->
    Some (min l1 l2, max u1 u2)
  ) r2
  ) r1

let meet =
  bind2 (fun (l1, u1) (l2, u2) ->
      if (l2 > u1 || l1 > u2) then
        None
      else
        from_bounds (max l1 l2) (min u1 u2)
  )

let widen =
  bind2 (fun (l1, u1) (l2, u2) ->
    let l = if l2 < l1
      then min_int
      else l1
    in
    let u = if u1 < u2
      then max_int
      else u1
    in
    Some (l, u)
  )

let is_infinite x =
  x == max_int || x == min_int

let add_overflow n x =
  if (is_infinite x) then x
  else x + n

let plus =
  bind2 (fun (a, b) (c, d) ->
    Some (add_overflow a c, add_overflow b d)
  )

let neg (r, alrm) =
  bind (fun (a, b) -> Some (- b, - a)) r, alrm

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

let mult_pp = bind2 (fun (a, b) (c,d) -> from_bounds (smul a c) (smul b d))
let mult_mp = bind2 (fun (a, b) (c,d) -> from_bounds (smul a d) (smul b c))
let mult_mm = bind2 (fun (a, b) (c,d) -> from_bounds (smul b d) (smul a c))

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

let is_in_range a b r =
  r <=% from_bounds a b

open Prog

let eval lookup _addr x =
  let bool_top   = from_bounds 0 1 in
  let bool_true  = from_bounds 1 1 in
  let bool_false = from_bounds 0 0 in
  let rec eval = function
  | Const (CInt n)            -> from_bounds n n, []
  | Lval ((L _| G _) as v',_) -> lookup v', []
  | Lval (Shift _,_) -> top, [] (* TODO maybe an alarm here ? *)
  | Op (Plus,  e1, e2) -> Alarm.combine plus (eval e1) (eval e2)
  | Op (Minus, e1, e2) -> Alarm.combine plus (eval e1) (neg (eval e2))
  | Not e' -> begin match fst (eval e') with
                           | Some (0,0)            -> bool_true
                           | Some (a,_) when a > 0 -> bool_false
                           | Some (_,b) when b < 0 -> bool_false
                           | _                     -> bool_top
              end, []
  | Op (Eq, e1, e2) -> Alarm.combine (fun r1  r2 ->
                           if meet r1 r2 = None then bool_false
                           else begin match (r1, r2) with
                           | Some (a, b), Some (c, d) when a = b
                                                       &&  b = c
                                                       &&  c = d -> bool_true
                           | _ -> bool_top
                           end
                      ) (eval e1) (eval e2)
  | Op (Mult, e1, e2) -> Alarm.combine mult (eval e1) (eval e2)
  | Belongs ((a, b), loc, e) ->
      let (r, a1) = eval e in
      let alrms =
        if (is_in_range a b r) then
          []
        else
            let reason =
                 "eval <"^Pcomp.Print.exp e^"> = "
                ^ to_string r ^ " </= [" ^ string_of_int a
                                   ^ ";" ^ string_of_int b ^ "]" in
            [loc, Alarm.Array_OOB, Some reason]
      in (r, a1@alrms)
  | Const Nil -> top, []
  | AddrOf _ -> top, []
  | Op (PlusPtr loc, _, _) -> top, [loc, Alarm.Ptr_OOB, None]
  | e -> failwith ( "range domain : unimplemented evaluator : "
                  ^ Pcomp.Print.exp e )
  in
  eval x

let guard _ _ e =
  match e with
  |      Op (Gt, Lval (v,_), Const (CInt n))  -> [v, meet (from_bounds (n+1) max_int)]
  |      Op (Gt, Const (CInt n), Lval (v,_))  -> [v, meet (from_bounds min_int (n-1))]
  |      Op (Eq, Lval (v,_), Const (CInt n))  -> [v, meet (from_bounds n n)]
  | Not (Op (Gt, Lval (v,_), Const (CInt n))) -> [v, meet (from_bounds min_int n)]
  | Not (Op (Gt, Const (CInt n), Lval (v,_))) -> [v, meet (from_bounds n max_int)]
  | Not (Op (Eq, Lval (v,_), Const (CInt n))) -> [v, function
                                                 | Some(x,y) when x = y && x = n -> None
                                                 | r -> r
                                                 ]
  | _ -> if Options.get Options.verbose then
           prerr_endline ("Unsupported guard statement : " ^ Pcomp.Print.exp e);
         []

let dom =
  { Domain.top         = top
  ; Domain.bottom      = bottom
  ; Domain.join        = join
  ; Domain.meet        = meet
  ; Domain.widen       = widen
  ; Domain.to_string   = to_string
  ; Domain.eval        = eval
  ; Domain.guard       = guard
  ; Domain.update      = None
  ; Domain.is_in_range = is_in_range
  }

