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

let may_cons h t = Utils.may (fun x -> h::x) t

let varcmp a b = match (a, b) with
  | Prog.L _ , Prog.G _ -> -1
  | Prog.G _ , Prog.L _ -> 1
  | Prog.G a', Prog.G b' -> String.compare a' b'
  | Prog.L a', Prog.L b' -> Pervasives.compare a' b'

module Alist : sig
  (** Association list *)
  type t

  val empty : t

  val singleton : Prog.var -> Range.t -> t

  val merge : (Range.t -> Range.t -> Range.t) -> t -> t -> t option

  val replace : Prog.var -> (Range.t -> Range.t) -> t -> t option

  val map : (Prog.var -> Range.t -> 'a) -> t -> 'a list

  val assoc : Prog.var -> t -> Range.t
end = struct
  (** Invariants : - list is sorted according to String.compare
   *               - there are no "top" elements
   *)
  type t = (Prog.var * Range.t) list

  let empty = []

  let singleton k x =
    (k, x)::[]

  let rec merge f x1 x2 = match (x1, x2) with
    | [], _  -> Some x2
    | _ , [] -> Some x1
    | (s1,r1)::t1, (s2,r2)::t2 ->
        match varcmp s1 s2 with
        | 0 -> begin
                 let r = f r1 r2 in
                 if r = Range.dom.bottom then None
                 else if r = Range.dom.top
                 then         merge f t1 t2
                 else
                   may_cons (s1, r) (merge f t1 t2)
               end
        | x when x < 0 -> may_cons (s1,r1) (merge f t1 x2)
        | _  (* > 0 *) -> may_cons (s2,r2) (merge f x1 t2)

  let rec replace var f = function
    | [] -> Some []
    | (s, r)::t -> match varcmp s var with
      | 0 -> begin
               let fr = f r in
               if fr = Range.dom.bottom then
                 None
               else Some ((s, fr)::t)
             end (* XXX may *)
      | x when x < 0 -> may_cons (s, r) (replace var f t)
      | _ (* > 0 *)  -> Some ((s, r)::t)

  let map f =
    List.map (fun (k, x) -> f k x)

  let rec assoc v = function
    | (v', x)::_ when v = v'                  -> x
    | (v', _)::t when varcmp v v' > 0 -> assoc v t
    | _ -> Range.dom.top

end

type t = Alist.t option

let bottom = None

(* bind : ('a -> 'b option) -> 'a option -> 'b option *)
let bind f = function
  | None   -> None
  | Some x -> f x

let bind2 f x y =
  match (x, y) with
  | None, _ -> y
  | _, None -> x
  | Some x', Some y' -> f x' y'

let bind2_bot f x y =
  match (x, y) with
  | None, _ -> None
  | _, None -> None
  | Some x', Some y' -> f x' y'

let join  = bind2     (Alist.merge Range.dom.join)
let meet  = bind2_bot (Alist.merge Range.dom.meet)
let widen = bind2_bot (Alist.merge Range.widen)

let singleton v r =
  Some (Alist.singleton v r)

let guard var f =
  bind (Alist.replace var f)

let set_var var r =
  bind (Alist.replace var (fun _ -> r))

let get_var v = function
  | None   -> Range.dom.bottom
  | Some x -> Alist.assoc v x

let to_string = function
  | None -> "(bot)"
  | Some x -> String.concat ", " (Alist.map (fun v r ->
                Pcomp.Print.var v^"->"^Range.dom.to_string r)
              x)

let yaml_dump = function
  | None   -> "bottom: yes"
  | Some x -> "value: {" ^(String.concat ", " (Alist.map (fun v r ->
      Pcomp.Print.var v ^": \""^Range.dom.to_string r^"\"") x))
      ^"}"
