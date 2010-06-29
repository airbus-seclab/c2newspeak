(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans, Olivier Levillain

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

type 'a t = 'a Stack.t

let create = Stack.create
let push   = Stack.push
let top    = Stack.top
let pop    = Stack.pop

let lookup p s =
  let s = Stack.copy s in
  let r = ref None in
  while (!r = None && not (Stack.is_empty s)) do
    let t = Stack.pop s in
      r := p t
  done;
  !r

let iter = Stack.iter

let height = Stack.length

let nth s n =
  let s = Stack.copy s in
  while (height s > n) do
    pop s;
  done;
  top s

let fold f init s =
  let r = ref init in
  iter (fun x -> r := f !r x) s;
  !r

