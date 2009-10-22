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
  | Some (a, b) -> Some (- a, - b)

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

