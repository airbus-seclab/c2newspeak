(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007  Charles Hymans
  
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

(** Simple language.
    All variables have type int.
*)

type t = {
  fnames: Newspeak.file list;           (** list of source file names *)
  globals: globals;                     (** program variables *)
  init: blk;                            (** initialization block of globals *)
  fundecs: (Newspeak.fid, fundec) Hashtbl.t;  
                                        (** table of all declared functions *)
  src_lang: Newspeak.src_lang;          (** source programming language *)
}

and globals = (string, gdecl) Hashtbl.t (** Table of global names to location *)

and gdecl = Newspeak.location

and fundec = blk

and blk = stmt list

and stmt = 
    Set                                 (** assignment *)
  | If                                  (** if then else *)
  | While                               (** while loop *)
  | Call                                (** function call *)
  | Assert                              (** assertion *)
