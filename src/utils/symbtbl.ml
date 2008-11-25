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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org
*)

type 'a t = {
  data: (string, 'a) Hashtbl.t;
  mutable env: string list;
  mutable backups: string list list;
}

let create () = {
  data = Hashtbl.create 100;
  env = [];
  backups = []
}

let find tbl x = Hashtbl.find tbl.data x

let bind tbl x d = 
  Hashtbl.add tbl.data x d;
  tbl.env <- x::tbl.env

let update tbl x d = Hashtbl.replace tbl.data x d

let save tbl = 
  tbl.backups <- tbl.env::tbl.backups;
  tbl.env <- []

let restore tbl =
  let rec restore env =
    match env with
	x::tl -> 
	  Hashtbl.remove tbl.data x;
	  restore tl
    | [] -> ()
  in
    restore tbl.env;
    match tbl.backups with
	hd::tl -> 
	  tbl.env <- hd;
	  tbl.backups <- tl
      | [] -> invalid_arg "Symbtbl.restore: unreachable code"

let mem tbl x = Hashtbl.mem tbl.data x
