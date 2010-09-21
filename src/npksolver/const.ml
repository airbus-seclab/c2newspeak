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

type t =
  | Top
  | Cst of int
  | Bot

let join a b = match (a, b) with
  | Bot, _   -> b
  | _  , Bot -> a
  | Cst x, Cst y when x = y -> a
  | _ -> Top

let meet a b = match (a, b) with
  | Top, _   -> b
  | _  , Top -> a
  | Cst x, Cst y when x = y -> a
  | _ -> Bot

let to_string = function
  | Top   -> "top"
  | Cst x -> "{"^string_of_int x^"}"
  | Bot   -> "bot"

let lift2 f (ra, aa) (rb, ab) =
  let (r, aop) = match (ra, rb) with
  | Bot   , _     -> Bot, []
  | _     , Bot   -> Bot, []
  | Cst x , Cst y -> let (rop, aop) = f x y in Cst rop, aop
  | Top   , _     -> Top, []
  | _     , Top   -> Top, []
  in r, (aop @ aa @ ab)

let liftv v = function
  | Bot -> Bot
  | _   -> v

open Prog

let is_in_range a b = function
  | Bot -> true
  | Cst c -> a <= c && c <= b
  | Top -> false

let eval lookup _sz e =
  let int_of_bool = function
    | true  -> 1
    | false -> 0
  in
  let eop o x y = match o with
    | Plus  -> x + y, []
    | Minus -> x - y, []
    | Div   -> x / y, []
    | Mult  -> x * y, []
    | Eq    -> int_of_bool (x = y), []
    | Gt    -> int_of_bool (x > y), []
    | PlusPtr loc -> x, [loc, Alarm.Ptr_OOB, None]
  in
  let rec eval = function
    | Const (CInt n) -> Cst n, []
    | Const Nil -> Top, []
    | AddrOf _ -> Top, []
    | Lval (v,_) -> lookup v, []
    | Not e -> liftv Top (fst (eval e)), []
    | Op (op, e1, e2) -> lift2 (eop op) (eval e1) (eval e2)
    | Belongs ((a, b), loc, e) ->
        let res = fst (eval e) in
        let alrm = 
          if (is_in_range a b res) then
            []
          else
            [loc, Alarm.Array_OOB, None]
        in
        (res, alrm)
  in
  eval e

let guard _ _ = function
  |      Op (Eq, Lval (v,_), Const (CInt n))  (* v == n *) -> [v, liftv (Cst n)]
  |      Op (Gt, Lval (v,_), Const _)         (* v >  n *)
  |      Op (Gt, Const _, Lval (v,_))         (* n >  v *)
  | Not (Op (Gt, Lval (v,_), Const _))        (* v <= n *)
  | Not (Op (Gt, Const _, Lval (v,_)))        (* n <= v *)
  | Not (Op (Eq, Lval (v,_), Const _))        (* v != n *) -> [v, liftv Top]
  | e -> failwith ("Unsupported guard statement : " ^ Pcomp.Print.exp e)

let dom =
  { Domain.top       = Top
  ; Domain.bottom    = Bot
  ; Domain.join      = join
  ; Domain.meet      = meet
  ; Domain.widen     = (fun _ _ -> failwith "Const:widen")
  ; Domain.is_in_range = is_in_range
  ; Domain.to_string = to_string
  ; Domain.eval      = eval
  ; Domain.guard     = guard
  ; Domain.update    = None
  ; Domain.top_array = (fun _ -> Top)
  ; Domain.where_does_it_point = (fun _ -> Domain.Where_I_dont_know)
  }
