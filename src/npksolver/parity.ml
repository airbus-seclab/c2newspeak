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

open Domain

(*        Top
 *      /     \
 *    Even    Odd
 *      \     /     
 *        Bot
 *)
type t =
  | Top
  | Even
  | Odd
  | Bot
 
let from_val x =
 if (x mod 2 = 0)
   then Even
   else Odd

let incl a b = match (a, b) with
  | Bot, _ -> true
  | _, Top -> true
  | _, Bot -> false
  | Top, _ -> false
  | _, _   -> a = b

let join a b = match (a, b) with
  | Bot, _ -> b
  | _, Bot -> a
  | Odd, Odd -> Odd
  | Even, Even -> Even
  | _ -> Top

let meet a b = match (a, b) with
  | Top, _ -> b
  | _, Top -> a
  | Odd, Odd -> Odd
  | Even, Even -> Even
  | _ -> Bot

open Prog
let eval_bop = function
  |  _          , Bot , _    -> Bot
  |  _          , _   , Bot  -> Bot
  | (Plus|Minus), Odd , Odd  -> Even
  | (Plus|Minus), Odd , Even -> Odd
  | (Plus|Minus), Even, Odd  -> Odd
  | (Plus|Minus), Even, Even -> Even
  |  Mult       , Odd , Odd  -> Odd
  |  Mult       , Odd , Even -> Even
  |  Mult       , Even, Odd  -> Even
  |  Mult       , Even, Even -> Even
  (* TODO improve precision ? *)
  | _ -> Top

let is_in_range _ _ _ = false

let rec eval loc lookup = function
  | Const (CInt c) -> from_val c
  | Const Nil -> Top
  | AddrOf _ -> Top
  | Lval (lv, _) -> lookup lv
  | Op (op, e1, e2) ->
      let r1 = eval loc lookup e1 in
      let r2 = eval loc lookup e2 in
      eval_bop (op, r1, r2)
  | Not e -> let r = eval loc lookup e in
    if r = Bot then Bot else Top
  | Belongs ((a, b), e) ->
      let res = eval loc lookup e in
      if (not (is_in_range a b res)) then
        Alarm.emit loc Alarm.ArrayOOB;
      res

let guard _loc = function
  | Op (Eq, Lval (lv, _), Const (CInt n)) -> [lv, meet (from_val n)]
  | _ -> []

let to_string = function
  | Top  -> "top"
  | Bot  -> "bot"
  | Odd  -> "odd"
  | Even -> "even"

let dom =
  { top         = Top
  ; bottom      = Bot
  ; incl        = incl
  ; join        = join
  ; meet        = meet
  ; widen       = (fun _ -> invalid_arg "parity:widen")
  ; to_string   = to_string
  ; eval        = eval
  ; guard       = guard
  ; update      = destructive_update
  ; is_in_range = is_in_range
  }

