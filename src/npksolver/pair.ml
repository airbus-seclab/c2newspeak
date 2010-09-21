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

open Domain
open Utils.Maybe
open Utils.Arrow
module Maybe_monad = Utils.Monad(Utils.Maybe)
open Maybe_monad

type ('a, 'b) t = ('a * 'b) option
 
let join  da db (xa, xb) (ya, yb) = (da.join  xa ya, db.join  xb yb)
let widen da db (xa, xb) (ya, yb) = (da.widen xa ya, db.widen xb yb)

let meet da db x y =
  x >>= fun (xa, xb) ->
  y >>= fun (ya, yb) ->
  let ma = da.meet xa ya in
  if (ma = da.bottom) then None
  else
    let mb = db.meet xb yb in
    if mb = db.bottom then
      None
    else Some (ma, mb)

let eval da db l addr e =
  let la e = maybe da.bottom fst (l e) in
  let lb e = maybe db.bottom snd (l e) in
  let (ra, alrm_a) = da.eval la addr e in
  let (rb, alrm_b) = db.eval lb addr e in
  let r = if ra = da.bottom || rb = db.bottom
    then None
    else Some (ra , rb)
  in
  (r, Alarm.meet alrm_a alrm_b)

let guard da db env addr e =
  let env_a x =
    match env x with
    | None -> invalid_arg "pair : guard : env_a"
    | Some (a, _) -> a
  in
  let env_b x =
    match env x with
    | None -> invalid_arg "pair : guard : env_b"
    | Some (_, b) -> b
  in
  let ga = List.map (fun (lv, f) -> (lv, fmap (first f))) (da.guard env_a addr e) in
  let gb = List.map (fun (lv, f) -> (lv, fmap (second f))) (db.guard env_b addr e) in
  ga@gb

let to_string da db =
  function
  | None -> "(bot)"
  | Some (a,b) -> "Pair (" ^ da.to_string a ^ ";"
                           ^ db.to_string b ^ ")"

let is_in_range d1 d2 a b = function
  | None -> true
  | Some (x1, x2) -> d1.is_in_range a b x1
                  || d2.is_in_range a b x2

let update da db sz l =
  function
  | None -> []
  | Some (xa, xb) ->
      let alrm_a =
        match da.update with
        | None -> []
        | Some f -> f sz l xa
      in
      let alrm_b =
        match db.update with
        | None -> []
        | Some f -> f sz l xb
      in
      Alarm.meet alrm_a alrm_b

let make da db =
  { top = Some (da.top, db.top)
  ; bottom = None
  ; join  = liftM2 (join  da db)
  ; widen = liftM2 (widen da db)
  ; to_string = to_string da db
  ; eval      = eval      da db
  ; meet      = meet      da db
  ; guard     = guard     da db
  ; update    = Some (update    da db)
  ; is_in_range = is_in_range da db
  ; top_array = (fun n -> Some (da.top_array n, db.top_array n))
  ; where_does_it_point = (fun _ -> Where_I_dont_know)
  }

