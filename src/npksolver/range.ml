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

type nat = Newspeak.Nat.t

let (<=:) a b = Newspeak.Nat.compare a b <= 0
let (<:)  a b = Newspeak.Nat.compare a b <  0
let (+:)  a b = Newspeak.Nat.add     a b

let min_nat = Newspeak.Nat.of_string "-2147483648"
let max_nat = Newspeak.Nat.of_string "2147483647"

let nat_min x y =
  if x <: y
    then x
    else y

let nat_max x y =
  if x <: y
    then y
    else x

type t = (nat * nat) option

let from_bounds a b =
  assert (a <=: b);
  Some (a, b)

let top = Some (min_nat, max_nat)

let bottom = None

let (<=%) a b = match (a, b) with
  | None       , _           -> true
  | Some _     , None        -> false
  | Some (a, b), Some (c, d) -> c <=: a && b <=: d

let join a b = match (a, b) with
  | None, _ -> b
  | _, None -> a
  | Some (l1, u1), Some (l2, u2) ->
      Some (nat_min l1 l2, nat_max u1 u2)

let meet a b = match (a, b) with
  | None, _ -> None
  | _, None -> None
  | Some (l1, u1), Some (l2, u2) ->
      begin
        if l2 <=: l1 then
          begin
            if u2 <: l1
              then None
              else from_bounds l1 (nat_min u1 u2)
          end
        else
          begin
            if u1 <: l2
              then None
              else from_bounds l2 (nat_min u1 u2)
          end
      end

let widen a b =
  match (a, b) with
  | None         , _             -> None
  | _            , None          -> None
  | Some (l1, u1), Some (l2, u2) -> let l = if l2 <: l1
                                      then min_nat
                                      else l1
                                    in
                                    let u = if u1 <: u2
                                      then max_nat
                                      else u1
                                    in
                                    Some (l, u)

let is_infinite x =
  x == max_nat || x == min_nat

let add_overflow n x =
  if (is_infinite x) then x
  else x +: n

let plus ab cd = match (ab, cd) with
  | None       , _           -> None
  | _          , None        -> None
  | Some (a, b), Some (c, d) -> Some (add_overflow a c, add_overflow b d)

let neg = function
  | None -> None
  | Some (a, b) -> Some (Newspeak.Nat.neg a, Newspeak.Nat.neg b)

let to_string =
  let string_of_nat_inf x =
    if      x = max_nat then "+oo"
    else if x = min_nat then "-oo"
    else Newspeak.Nat.to_string x
  in function
  | None        -> "(bot)"
  | Some (a, b) when a = min_nat && b = max_nat -> "(top)"
  | Some (a, b) when a = b -> "{" ^ Newspeak.Nat.to_string a ^ "}"
  | Some (a, b)            -> "[" ^ string_of_nat_inf      a ^ ";"
                                  ^ string_of_nat_inf      b ^ "]"

