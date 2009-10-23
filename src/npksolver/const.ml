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
  | Cst x -> string_of_int x
  | Bot   -> "bot"

let dom = { Domain.top       = Top
          ; Domain.bottom    = Bot
          ; Domain.from_val  = (fun x -> Cst x)
          ; Domain.incl      = (<=%)
          ; Domain.join      = join
          ; Domain.meet      = meet
          ; Domain.to_string = to_string
          }
