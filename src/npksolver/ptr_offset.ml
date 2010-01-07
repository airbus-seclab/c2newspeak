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
  | PointsTo of Prog.addr
  | Null
  | Bot

type 'a t =
  { var : ptr
  ; offset : 'a
  }

let top dom = { var = Top ; offset = dom.top }

let bot dom = { var = Bot ; offset = dom.bottom }

let var_join = function
  | Top, _   -> Top
  | _  , Top -> Top
  | PointsTo x, PointsTo y when x = y -> PointsTo x
  | PointsTo _, PointsTo _            -> Top
  | Null, Null -> Null
  | Null, PointsTo _ -> Top
  | PointsTo _, Null -> Top
  | x, Bot -> x
  | Bot, y -> y

let var_meet = function
  | x  , Top -> x
  | Top, y   -> y
  | Bot, _   -> Bot
  | _, Bot   -> Bot
  | PointsTo x, PointsTo y when x = y -> PointsTo x
  | PointsTo _, PointsTo _            -> Bot
  | Null, PointsTo _ -> Bot
  | PointsTo _, Null -> Bot
  | Null, Null -> Null

let join dom x y =
  { var    = var_join (x.var, y.var)
  ; offset = dom.join x.offset y.offset
  }

let meet dom x y =
  { var    = var_meet (x.var, y.var)
  ; offset = dom.meet x.offset y.offset
  }

let incl dom x y =
  match (x.var, y.var) with
  | Bot, _ -> true
  | _, Top -> true
  | _, Bot -> false
  | Top, _ -> false
  | Null, Null -> true
  | PointsTo x', PointsTo y' when x' = y' ->
      dom.incl x.offset y.offset
  | PointsTo _, PointsTo _ -> false
  | PointsTo _ , Null -> false
  | Null , PointsTo _ -> false

let lift f x =
  { x with offset = f x.offset }

let rec eval dom env addr_of =
  function
  | Op (PlusPtr _loc, lv, off) ->
      let (r, alrm1) = eval dom env addr_of lv in
      (* comprehending environment for { x + off / x <- r.off } *)
      let c_env = function
        | G "!" -> r.offset
        | v -> (env v).offset
      in
      let (off', alrm2) = dom.eval c_env addr_of (Op (Plus, Lval (G "!", Int), off)) in
      { var = r.var
      ; offset = off'
      }, alrm1@alrm2
  | Const (CInt 0) ->
      { var = Null
      ; offset = dom.top
      }, []
  | AddrOf v ->
      { var = PointsTo (addr_of v)
      ; offset = const dom 0
      }, []
  | Lval (lv, _sc) ->
      env lv, []
  | exp -> prerr_endline
             ( "Ptr_offset : invalid expression : "
             ^ Pcomp.Print.exp exp ); top dom, []

let guard dom env addr_of =
  let liftg = List.map (fun (l, x) -> (l, lift x)) in
  let l_env x = (env x).offset in
  function
    (* (var +p off > ptr) *)
  | Op (Gt, Op(PlusPtr _loc1, AddrOf (var), off), (Lval (ptr, _) as lv)) ->
      begin
      let r = env ptr in
      match r.var with
      | PointsTo var' when var' = addr_of var ->
          let rew_guard =
            Op (Gt, off, lv)
          in
          liftg (dom.guard l_env addr_of rew_guard)
      | _ -> []
      end
    (* ! (var +p off > ptr) *)
  | Not (Op (Gt, Op(PlusPtr _loc1, AddrOf (var), off), (Lval (ptr, _) as lv))) ->
      begin
      let r = env ptr in
      match r.var with
      | PointsTo var' when var' = addr_of var ->
          let rew_guard =
            Not (Op (Gt, off, lv))
          in
          liftg (dom.guard l_env addr_of rew_guard)
      | _ -> []
      end
  | exp -> liftg (dom.guard l_env addr_of exp)

let ptr_update dom size_of loc new_value =
  let alarm ?reason _ =
    [loc, Alarm.Ptr_OOB, reason]
  in
  begin
  match new_value.var with
  | Null -> alarm ~reason:"points-to Null" ()
  | Top  -> alarm ~reason:("points-to Top (off = "^dom.to_string new_value.offset^")")  ()
  | Bot  -> alarm ~reason:"points-to Bot"  ()
  | PointsTo var ->
      let sz = size_of var in
      if (dom.is_in_range 0 sz new_value.offset) then
        []
      else
        alarm ~reason:(dom.to_string new_value.offset
               ^ " </= [0;" ^ string_of_int sz ^ "]") ()
  end

let make dom =
  { top         = top dom
  ; bottom      = bot dom
  ; incl        = incl dom
  ; join        = join dom
  ; meet        = meet dom
  ; widen       = (fun _ -> invalid_arg "ptr_offset : widen")
  ; to_string   = (fun r -> "PtrOff (" ^ dom.to_string r.offset ^ ")")
  ; eval        = eval  dom
  ; guard       = guard dom
  ; update      = Some (ptr_update dom)
  ; is_in_range = (fun _ _ _ -> false)
  }
