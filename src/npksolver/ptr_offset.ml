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
open Prog

type ptr =
  | Top
  | PointsTo of Prog.var
  | Null
  | Bot

type 'a t =
  { var : ptr
  ; offset : 'a
  }

let ptr_sup = function
  | Top, _   -> Top
  | _  , Top -> Top
  | PointsTo x, PointsTo y when x = y -> PointsTo x
  | PointsTo _, PointsTo _            -> Top
  | Null, Null -> Null
  | Null, PointsTo _ -> Top
  | PointsTo _, Null -> Top
  | x, Bot -> x
  | Bot, y -> y

let ptr_inf = function
  | x  , Top -> x
  | Top, y   -> y
  | Bot, _   -> Bot
  | _, Bot   -> Bot
  | PointsTo x, PointsTo y when x = y -> PointsTo x
  | PointsTo _, PointsTo _            -> Bot
  | Null, PointsTo _ -> Bot
  | PointsTo _, Null -> Bot
  | Null, Null -> Null

let off_map2 pcmp f x y =
  { var = pcmp (x.var, y.var)
  ; offset = f (x.offset) (y.offset)
  }

let lift f x =
  { x with offset = f x.offset }

let rec eval dom env =
  let ttop = { var = Top ; offset = dom.top } in
  function
  | Op (PlusPtr _loc, lv, off) ->
      let r = eval dom env lv in
      (* comprehending environment for { x + off / x <- r.off } *)
      let c_env = function
        | G "!" -> r.offset
        | _ -> invalid_arg "ptr_offset.eval"
      in
      let off' = dom.eval c_env (Op (Plus, Lval (G "!", Int), off)) in
      { r with offset = off'}
  | Const (CInt _) -> ttop
  | AddrOf v ->
      { var = PointsTo (Pcomp.to_var dom v)
      ; offset = const dom 0
      }
  | Lval (lv, _sc) ->
      { var = PointsTo (Pcomp.to_var dom lv)
      ; offset = const dom 0
      }
  | exp -> prerr_endline
            ( "Ptr_offset : invalid expression : "
            ^ Pcomp.Print.exp exp); ttop

let guard dom exp =
  List.map (fun (l, x) -> (l, lift x)) (dom.guard exp)

let ptr_update dom sizes lv ~old_value ~new_value =
  ignore old_value;
  let alarm ?reason _ =
    Alarm.emit Newspeak.unknown_loc Alarm.PtrOOB;
    match reason with
    | None   -> ()
    | Some r ->
        if (Options.get Options.verbose) then
          prerr_endline ("Reason : " ^ r)
  in
  begin
  match new_value.var with
  | Null -> alarm ~reason:"points-to Null" ()
  | Top  -> alarm ~reason:"points-to Top"  ()
  | Bot  -> alarm ~reason:"points-to Bot"  ()
  | PointsTo var ->
      let sz = sizes var in
      if not (dom.incl new_value.offset (const dom sz)) then
        alarm ()
  end ;
  (lv, new_value)

let make dom =
  { top         = { var = Top; offset = dom.top    }
  ; bottom      = { var = Bot; offset = dom.bottom }
  ; incl        = (fun x y -> dom.incl x.offset y.offset)
  ; join        = off_map2 ptr_sup dom.join
  ; meet        = off_map2 ptr_inf dom.meet
  ; widen       = off_map2 ptr_sup dom.widen
  ; to_string   = (fun r -> "PtrOff (" ^ dom.to_string r.offset ^ ")")
  ; eval        = eval  dom
  ; guard       = guard dom
  ; update      = ptr_update dom
  ; is_in_range = (fun _ _ _ -> false)
  }
