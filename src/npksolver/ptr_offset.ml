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

(*
 * TODO for soundness
 * check that var is never changed (after the first time, always return top)
 *)
type 'a t =
  { var : unit
  ; offset : 'a
  }

let shift x =
  { var = ()
  ; offset = x
  }

let unshift x =
  (x).offset

let off_map2 f x y =
  shift (f (unshift x) (unshift y))

let lift f x =
  shift (f (unshift x))

let eval dom env exp =
  shift (dom.eval (fun x -> unshift (env x)) exp)

let guard dom exp =
  List.map (fun (l, x) -> (l, lift x)) (dom.guard exp)

let make dom =
  { top       = { var = (); offset = dom.top }
  ; bottom    = { var = (); offset = dom.bottom }
  ; incl      = (fun x y -> dom.incl x.offset y.offset)
  ; join      = off_map2 dom.join
  ; meet      = off_map2 dom.meet
  ; widen     = off_map2 dom.widen
  ; to_string = (fun r -> "PtrOff (" ^ dom.to_string r.offset ^ ")")
  ; eval      = eval  dom
  ; guard     = guard dom
  ; update    = destructive_update
  ; is_in_range = (fun _ _ _ -> false)
  }
