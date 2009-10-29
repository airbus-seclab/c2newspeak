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

type t =
  | Top
  | Cst of int
  | Bot

let (<=%) a b = match (a, b) with
  | Bot  , _     -> true
  | _    , Top   -> true
  | Cst x, Cst y -> x = y
  | _            -> false

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

let lift2 f a b = match (a, b) with
  | Bot   , _     -> Bot
  | _     , Bot   -> Bot
  | Cst x , Cst y -> Cst (f x y)
  | Top   , _     -> Top
  | _     , Top   -> Top

let eval lookup e =
  let int_of_bool = function
    | true  -> 1
    | false -> 0
  in
  let eop = function
    | Prog.Plus -> (+)
    | Prog.Minus -> (-)
    | Prog.Eq -> (fun x y -> int_of_bool (x = y))
    | Prog.Gt -> (fun x y -> int_of_bool (x > y))
    | Prog.Div -> (/)
    | Prog.Mult -> fun x y -> x * y
  in
  let rec eval = function
    | Prog.Const n -> Cst n
    | Prog.Var v -> lookup v
    | Prog.Not e -> begin match (eval e) with
                    | Bot -> Bot
                    | _   -> Top
                    (* TODO lift *)
                    end
    | Prog.Op (op, e1, e2) -> lift2 (eop op) (eval e1) (eval e2)
  in
  eval e

let guard _e = None

let dom = { Domain.top       = Top
          ; Domain.bottom    = Bot
          ; Domain.from_val  = (fun x -> Cst x)
          ; Domain.incl      = (<=%)
          ; Domain.join      = join
          ; Domain.meet      = meet
          ; Domain.widen     = (fun _a _b -> failwith "Const:widen")
          ; Domain.to_string = to_string
          ; Domain.eval      = eval
          ; Domain.guard     = guard
          }
