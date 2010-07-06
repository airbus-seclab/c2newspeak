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

type 'a t = (string, 'a array) Hashtbl.t

let make sizes init =
  let h = Hashtbl.create 0 in
  List.iter (fun (str, sz) ->
    Hashtbl.add h str (Array.make sz init)
  ) sizes;
  h

let get h str =
  Array.get (Hashtbl.find h str)

let set h str =
  Array.set (Hashtbl.find h str)

let array_foldi f a x0 =
  let arr_i = Array.mapi (fun i x -> (i,x)) a in
  let l = Array.to_list arr_i in
  List.fold_left (fun r (i, x) -> f i x r) x0 l

let fold f =
  Hashtbl.fold (fun str -> array_foldi (f str))

let map f h =
  let l =  Hashtbl.fold (fun str a l -> (str, a)::l) h [] in
  let h' = Hashtbl.create 0 in
  List.iter (fun (s, a) -> Hashtbl.add h' s (Array.map f a)) l;
  h'

let size h str =
  Array.length(Hashtbl.find h str)
