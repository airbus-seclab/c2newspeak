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

open Interval
open Utils

let top = top_int

let bottom = bottom_int

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
  | Op (Minus, e1, e2) -> Alarm.combine plus (eval e1) ((Arrow.first neg) (eval e2))
  | Not e' -> begin match fst (eval e') with
                           | Interval (0,0)            -> bool_true
                           | Interval (a,_) when a > 0 -> bool_false
                           | Interval (_,b) when b < 0 -> bool_false
                           | _                     -> bool_top
              end, []
  | Op (Eq, e1, e2) -> Alarm.combine (fun r1  r2 ->
                           if meet r1 r2 = bottom then bool_false
                           else begin match (r1, r2) with
                           | Interval (a, b), Interval (c, d)
                               when a = b &&  b = c &&  c = d -> bool_true
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
                                                 | Interval(x,y) when x = y && x = n -> bottom
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
  ; Domain.top_array = (fun _ -> top)
  ; Domain.where_does_it_point = (fun _ -> Domain.Where_I_dont_know)
  }
