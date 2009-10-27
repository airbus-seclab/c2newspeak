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
  (** Invariants : - list is sorted according to varcmp
   *               - there are no "top" elements
   *)
  type t = (Prog.var * Range.t) list

  let empty = []

  let singleton k x =
    assert(x<>Range.dom.top);
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
               else if fr = Range.dom.top then
                 Some t
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

type box = { store : Alist.t ; esp : int }

type t = box option

let top = Some {store = Alist.empty ; esp = 0}

let bottom = None

let bind f = function
  | None   -> None
  | Some x ->
      begin
        match f (x.store) with
        | None -> None
        | Some y -> Some { x with store = y }
      end

let bind2 f x y =
  match (x, y) with
  | None, _ -> y
  | _, None -> x
  | Some x', Some y' ->
      begin
        match f x'.store y'.store with
        | None -> None
        | Some z -> Some { x' with store = z }
      end


let bind2_bot f x y =
  match (x, y) with
  | None, _ -> None
  | _, None -> None
  | Some x', Some y' ->
      begin
        match f x'.store y'.store with
        | None -> None
        | Some z -> Some { x' with store = z }
      end

let join  = bind2     (Alist.merge Range.dom.join)
let meet  = bind2_bot (Alist.merge Range.dom.meet)
let widen = bind2_bot (Alist.merge Range.widen)

let singleton v r =
  Some { store = Alist.singleton v r ; esp = 0 }

let guard var f =
  bind (Alist.replace var f)

let set_var v r =
  let set_replace esp = function
    | Prog.G v -> Prog.G v
    | Prog.L n -> Prog.L (esp - n)
  in
  function
  | None   -> None
  | Some x ->
      begin
        match Alist.merge (fun a _ -> a) (Alist.singleton (set_replace x.esp v) r) (x.store) with
        | None   -> None
        | Some y -> Some { x with store = y }
      end

let push = function
  | None   -> None
  | Some x -> Some { x with esp = x.esp + 1 }

let pop  = function
  | None   -> None
  | Some x ->
      let s = Alist.replace (Prog.L x.esp) (fun _ -> Range.dom.top) x.store in
      match s with None -> None
      | Some s' -> 
      Some { store = s' ; esp = x.esp - 1 }

let get_var v = function
  | None   -> Range.dom.bottom
  | Some x ->
      begin
        match v with
        | Prog.G _ -> Alist.assoc v x.store
        | Prog.L n -> Alist.assoc (Prog.L (x.esp - n)) x.store
      end

let to_string = function
  | None -> "(bot)"
  | Some x -> String.concat ", " (Alist.map (fun v r ->
                Pcomp.Print.var v^"->"^Range.dom.to_string r)
              x.store)

let yaml_dump =
  function
  | None   -> "bottom: yes"
  | Some x -> "value: {" ^(String.concat ", " (Alist.map (fun v r ->
      Pcomp.Print.var v ^": \""^Range.dom.to_string r^"\"") x.store))
      ^"}"
