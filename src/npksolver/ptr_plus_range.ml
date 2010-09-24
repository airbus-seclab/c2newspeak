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
open Utils
open Prog

type t =
  | Top
  | Array
  | Range    of int Interval.t
  | Pointsto of addr * int Interval.t
  | Null
  | Bottom

let join ?(with_widening=false) a b = match (a, b) with
  | Top, _ -> Top
  | _, Top -> Top
  | Bottom, y -> y
  | x, Bottom -> x
  | Range x, Range y ->
      if with_widening
        then Range (Range.dom.widen x y)
        else Range (Range.dom.join  x y)
  | Pointsto (x,ox), Pointsto (y,oy) when x = y ->
      if with_widening
        then Pointsto (x, Range.dom.widen ox oy)
        else Pointsto (x, Range.dom.join  ox oy)
  | Pointsto _, Pointsto _ (* when x /= y *) -> Top
  | (Pointsto _|Null), Range _ -> assert false(*; Top *)
  | Range _, (Pointsto _|Null) -> assert false(*; Top *)
  | Array, (Range _| Null| Pointsto _) -> assert false(*; Top *)
  | (Range _| Null| Pointsto _), Array -> assert false(*; Top *)
  | Null, Null -> Null
  | Null, Pointsto _ -> assert false;(*Top*)
  | Pointsto _, Null -> Top
  | Array, Array -> Array

let normalize = function
  | Range Interval.Empty -> Bottom
  | x -> x

let meet a b = match (a, b) with
  | Top, y -> y
  | x, Top -> x
  | (Bottom|Range Interval.Empty), _ -> Bottom
  | _, (Bottom|Range Interval.Empty) -> Bottom
  | Range x, Range y -> normalize (Range (Range.dom.meet x y))
  | Pointsto (x,ox), Pointsto (y,oy) when x = y ->
      Pointsto (x, Range.dom.meet ox oy)
  | Pointsto _, Pointsto _ (* when x /= y *) -> Bottom
  | (Pointsto _|Null), Range _ -> assert false(*; Bottom *)
  | Range _, (Pointsto _|Null) -> assert false(*; Bottom *)
  | Array, (Range _| Null| Pointsto _) -> assert false(*; Bottom *)
  | (Range _| Null| Pointsto _), Array -> assert false(*; Bottom *)
  | Null, Null -> Null
  | Null, Pointsto _ -> Bottom
  | Pointsto _, Null -> Bottom
  | Array, Array -> Array

let to_string = function
  | Top            -> "T"
  | Range     r    -> Range.dom.to_string r
  | Pointsto (a,o) -> "→ " ^ Pcomp.Print.addr a ^ "@" ^ Range.dom.to_string o
  | Null           -> "Null"
  | Bottom         -> "⊥"
  | Array          -> "array"

let is_in_range a b = function
  | Top -> false
  | Bottom -> true
  | Range r -> Range.dom.is_in_range a b r
  | Pointsto _ | Null | Array -> invalid_arg "ptr+range ∷ is_in_range"

let unsafe_map2_range op x y = match (x,y) with
  | Top, _ -> Top
  | _, Top -> Top
  | Range a, Range b -> Range (op a b)
  | _ -> invalid_arg ( "ptr+range ∷ unsafe_map2_range : ("
                     ^ to_string x ^ ", " ^ to_string y ^ ")")

let range a b =
  Range (Interval.from_bounds a b)

let singleton n = range n n

let eval env addr e =
  let bool_top   = range 0 1 in
  let bool_true  = singleton 1 in
  let bool_false = singleton 0 in
  let rec eval = function
  | Belongs ((a,b),loc,exp) ->
      let (r, alrms) = eval exp in
      if (is_in_range a b r)
        then (r, alrms)
        else (r, (loc,Alarm.Array_OOB,None)::alrms)
  | Const (CInt n) -> (singleton n, [])
  | Const Nil -> (Null, [])
  | Lval (lv, _ty) -> (env lv, [])
  | Op (Eq, e1, e2) ->
      let (r1, a1) = eval e1 in
      let (r2, a2) = eval e2 in
      let r =
        if meet r1 r2 = Bottom
          then bool_false
          else
            begin
              match (r1, r2) with
                |   Range (Interval.Interval (a, b))
                  , Range (Interval.Interval (c, d))
                      when a = b
                       &&  b = c
                       &&  c = d
                    -> bool_true
                | _ -> bool_top
            end
      in
      (r, a1@a2)
  | Op (Mult, e1, e2) ->
      let (r1, a1) = eval e1 in
      let (r2, a2) = eval e2 in
      (unsafe_map2_range Interval.mult r1 r2, a1@a2)
  | Op (Plus, e1, e2) ->
      let (r1, a1) = eval e1 in
      let (r2, a2) = eval e2 in
      (unsafe_map2_range Interval.plus r1 r2, a1@a2)
  | AddrOf lv -> (Pointsto (addr lv, Interval.from_bounds 0 0), [])
  | Op (PlusPtr _loc, e1, e2) ->
      begin
        let (r1,a1) = eval e1 in (* pointer *)
        let (r2,a2) = eval e2 in (* offset *)
        let (r,a) = match (r1, r2) with
          (*| (Top, _) -> Top,[loc,Alarm.Ptr_OOB, None]*)
          | (Pointsto (lv,_), Top)      -> Pointsto (lv, Range.dom.top),[]
          | (Pointsto (lv,o), Range ro) -> Pointsto (lv, Interval.plus o ro),[]
          | _ -> invalid_arg ( "ptr+range ∷ eval ∷ PlusPtr ("
                             ^ to_string r1 ^ ", " ^ to_string r2 ^ ")"
                             )
        in (r, a@a1@a2)
      end
  | e ->
      let reason = "ptr+range : eval " ^ Pcomp.Print.exp e in
      (Top, [ Newspeak.unknown_loc
            , Alarm.Assertion_failed reason
            , None
            ]
      )
  in
  eval e

let fmap_range_t (f:int Interval.t -> int Interval.t)
                 :  t -> t
  = function
    | Range r -> Range (f r)
                   (*
    | (Top | Bottom | Null | Pointsto _) as x -> x
                   *)
    | Pointsto(a,o) -> Pointsto (a, f o) (* FIXME this should be valid only for updates on a *)
    | x -> invalid_arg ("fmap_range_t ∷ " ^ to_string x)

let rewrite_guard g addr_of var = function
  | Pointsto (var',_) when var' = addr_of var ->
      let env' _ = invalid_arg "ptr_plus_range ∷ env'" in
      let gs = Range.dom.guard env' addr_of g in
      List.map (Arrow.second fmap_range_t) gs
  | _ -> []

let guard env addr_of =
  function
  |      Op (Eq, Lval (v,_), Const (CInt n))  -> [v, meet (singleton n)]
  |      Op (Gt, Const (CInt n), Lval (v,_))  -> [v, meet (range min_int (n-1))]
  | Not (Op (Gt, Const (CInt n), Lval (v,_))) -> [v, meet (range n max_int)]
  | Not (Op (Eq, Lval (v,_), Const (CInt n))) ->
      [ v
      , function
        | Range (Interval.Interval(x,y)) when x = y && x = n -> Bottom
        | r -> r
      ]
  | Op (Gt, Op (PlusPtr _loc, AddrOf var, off), (Lval (ptr, _) as lv)) ->
      rewrite_guard (Op (Gt, off, lv)) addr_of var (env ptr)
  | Not (Op (Gt, Op(PlusPtr _loc1, AddrOf (var), off), (Lval (ptr, _) as lv))) ->
      rewrite_guard (Not (Op (Gt, off, lv))) addr_of var (env ptr)
  | e -> if Options.get Options.verbose then
           prerr_endline ("Unsupported guard statement : " ^ Pcomp.Print.exp e);
         []

let update_check (sz   :(Prog.addr -> int))
                 (loc  : Newspeak.location)
                 : 'a -> Alarm.t list
  = function
    | Null -> []
    | Range _ -> []
    | Pointsto (x,o) ->
        let s = sz x in
        (* Note : a pointer may point to end of region + 1, hence [0;s] *)
        if (Interval.(<=%) o (Interval.from_bounds 0 s))
          then []
          else [( loc
                , Alarm.Ptr_OOB
                , Some ( Range.dom.to_string o
                       ^ " </= [0;"
                       ^ string_of_int (s - 1)
                       ^ "]"
                       )
                )]
    | Top -> [loc, Alarm.Assertion_failed "update check on Top", None]
    | Array -> []
    | x -> invalid_arg ("ptr+range ∷ update_check ∷ " ^ to_string x)

let where_does_it_point = function
  | Top   -> Where_I_dont_know
  | Array -> Where_I_dont_know
  | Null -> Where_on_null
  | Bottom -> Where_nowhere
  | Range _ -> Where_nowhere
  | Pointsto (addr, off) -> Where_on (addr, off)

let dom =
  { top         = Top
  ; bottom      = Bottom
  ; join        = join
  ; meet        = meet
  ; widen       = join ~with_widening:true
  ; to_string   = to_string
  ; is_in_range = is_in_range
  ; eval        = eval
  ; guard       = guard
  ; update      = Some update_check
  ; top_array   = (fun _n -> Array)
  ; where_does_it_point = where_does_it_point
  }
