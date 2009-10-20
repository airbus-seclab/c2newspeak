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

module Alist : sig
  (** Association list *)
  type t

  val empty : t

  val singleton : string -> Range.t -> t

  val apply : (Range.t -> Range.t -> Range.t) -> t -> t -> t

  val replace : string -> (Range.t -> Range.t) -> t -> t

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

  let rec apply f x1 x2 = match (x1, x2) with
    | [], _  -> x2
    | _ , [] -> x1
    | (s1,r1)::t1, (s2,r2)::t2 ->
        match String.compare s1 s2 with
        | 0 -> begin
                 let r = f r1 r2 in
                 if r = Range.top
                 then         apply f t1 t2
                 else (s1,r)::apply f t1 t2
               end
        | x when x < 0 -> (s1,r1)::apply f t1 x2
        | _  (* > 0 *) -> (s2,r2)::apply f x1 t2

  let rec replace var f = function
    | [] -> []
    | (s, r)::t -> match String.compare s var with
      | 0 -> (s, f r)::t
      | x when x < 0 -> (s, r)::replace var f t
      | _ (* > 0 *)  -> (s, r)::t

  let map f =
    List.map (fun (k, x) -> f k x)

  let rec assoc v = function
    | (v', x)::_ when v = v'                  -> x
    | (v', _)::t when String.compare v v' > 0 -> assoc v t
    | _ -> raise Not_found

end

type t = Alist.t

let bottom = Alist.empty

let bottom_var var =
  Alist.singleton var (Range.bottom)

let from_bounds var min max =
  Alist.singleton var (Range.from_bounds min max)

let join  = Alist.apply Range.join
let meet  = Alist.apply Range.meet
let widen = Alist.apply Range.widen

let shift var n x =
  Alist.replace var (Range.shift n) x

let add_bound ?(min=min_int) ?(max=max_int) var x =
  Alist.replace var (Range.add_bound ~min ~max) x

let get_var = Alist.assoc

let to_string x =
  String.concat ", " (Alist.map (fun s r -> s^"->"^Range.to_string r) x)

let yaml_dump x =
  "{"^
  String.concat ", " (Alist.map (fun s r -> s^": \""^Range.to_string r^"\"") x)
  ^"}"
