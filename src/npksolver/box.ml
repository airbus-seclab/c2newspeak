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

let may_cons h t = Utils.may (fun x -> h::x) t

(* XXX factor *)

let min_nat = Newspeak.Nat.of_string "-2147483648"
let max_nat = Newspeak.Nat.of_string "2147483647"

module Alist : sig
  (** Association list *)
  type t

  val empty : t

  val singleton : string -> Range.t -> t

  val merge : (Range.t -> Range.t -> Range.t) -> t -> t -> t option

  val replace : string -> (Range.t -> Range.t) -> t -> t option

  val map : (string -> Range.t -> 'a) -> t -> 'a list

  val assoc : string -> t -> Range.t
end = struct
  (** Invariants : - list is sorted according to String.compare
   *               - there are no "top" elements
   *)
  type t = (string * Range.t) list

  let empty = []

  let singleton k x =
    (k, x)::[]

  let rec merge f x1 x2 = match (x1, x2) with
    | [], _  -> Some x2
    | _ , [] -> Some x1
    | (s1,r1)::t1, (s2,r2)::t2 ->
        match String.compare s1 s2 with
        | 0 -> begin
                 let r = f r1 r2 in
                 if r = Range.bottom then None
                 else if r = Range.top
                 then         merge f t1 t2
                 else
                   may_cons (s1, r) (merge f t1 t2)
               end
        | x when x < 0 -> may_cons (s1,r1) (merge f t1 x2)
        | _  (* > 0 *) -> may_cons (s2,r2) (merge f x1 t2)

  let rec replace var f = function
    | [] -> Some []
    | (s, r)::t -> match String.compare s var with
      | 0 -> begin
               let fr = f r in
               if fr = Range.bottom then
                 None
               else Some ((s, fr)::t)
             end (* XXX may *)
      | x when x < 0 -> may_cons (s, r) (replace var f t)
      | _ (* > 0 *)  -> Some ((s, r)::t)

  let map f =
    List.map (fun (k, x) -> f k x)

  let rec assoc v = function
    | (v', x)::_ when v = v'                  -> x
    | (v', _)::t when String.compare v v' > 0 -> assoc v t
    | _ -> Range.top

end

type t = Alist.t option

let bottom = None

let from_bounds var min max =
  Some (Alist.singleton var (Range.from_bounds min max))

(* Haskell's (=<<) *)
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

let join  = bind2     (Alist.merge Range.join)
let meet  = bind2_bot (Alist.merge Range.meet)
let widen = bind2_bot (Alist.merge Range.widen)

let shift var n =
  bind (Alist.replace var (Range.shift n))

let add_bound ?(min=min_nat) ?(max=max_nat) var x =
  meet (from_bounds var min max) x

let assign_var ~src ~dest =
  bind (fun x ->
    let value = Alist.assoc src x in
    Alist.replace dest (fun _ -> value) x
  )

let set_var var n =
  bind (Alist.replace var (fun _ -> Range.from_bounds n n))

let get_var v = function
  | None   -> raise Not_found
  | Some x -> Alist.assoc v x

let to_string = function
  | None -> "(bot)"
  | Some x -> String.concat ", " (Alist.map (fun s r -> s^"->"^Range.to_string r) x)

let yaml_dump = function
  | None   -> "bottom: yes"
  | Some x -> "value: {" ^(String.concat ", " (Alist.map (fun s r ->
      s ^": \""^Range.to_string r^"\"") x))
      ^"}"
