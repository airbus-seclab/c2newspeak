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
open Utils
open Utils.Lift

type ('a, 'b) t = ('a * 'b) option
 
let incl da db x y =
  with_default false (
    x >>= fun (xa, xb) ->
    y >>= fun (ya, yb) ->
    return ( da.incl xa ya
          || db.incl xb yb )
  )

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

let eval da db l e =
  let la e = maybe da.bottom fst (l e) in
  let lb e = maybe db.bottom snd (l e) in
  let ra = da.eval la e in
  if ra = da.bottom
    then None
    else
      let rb = db.eval lb e in
      if rb = db.bottom
        then None
        else Some ( da.eval la e
                  , db.eval lb e )

let guard da db e =
  let prom_l f (a, b) = (  a, f b) in
  let prom_r f (a, b) = (f a,   b) in
  let ga = List.map (fun (lv, f) -> (lv, may (prom_r f))) (da.guard e) in
  let gb = List.map (fun (lv, f) -> (lv, may (prom_l f))) (db.guard e) in
  ga@gb

let to_string da db =
  function
  | None -> "(bot)"
  | Some (a,b) -> "Pair (" ^ da.to_string a ^ ";"
                           ^ db.to_string b ^ ")"

let make da db =
  { top = Some (da.top, db.top)
  ; bottom = None
  ; join  = liftM2 (join  da db)
  ; widen = liftM2 (widen da db)
  ; to_string = to_string da db
  ; eval      = eval      da db
  ; incl      = incl      da db
  ; meet      = meet      da db
  ; guard     = guard     da db
  ; update    = destructive_update
  }

