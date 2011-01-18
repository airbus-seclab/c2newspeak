(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007, 2011  Charles Hymans, Olivier Levillain, Sarah Zennou
  
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

  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah(dot)zennou(at)eads(dot)net
*)

type t = {
  size_of_byte	    : int;
  max_sizeof	    : int;
  max_array_length  : int;
  size_of_char	    : int;
  size_of_short	    : int;
  size_of_int	    : int;
  size_of_long	    : int;
  size_of_longlong  : int;
  size_of_ptr	    : int;
  size_of_float	    : int;
  size_of_double    : int;
  size_of_longdouble: int;
}

let byte = 8 
let max_sizeof = 1073741823 
let c 	 = {
    size_of_byte       = byte;
    max_sizeof 	       = max_sizeof;
    max_array_length   = max_sizeof / byte;
    size_of_char       = 1* byte;
    size_of_short      = 2* byte;
    size_of_int        = 4* byte;
    size_of_long       = 4*byte;
    size_of_longlong   = 8*byte;
    size_of_ptr        = 4*byte;
    size_of_float      = 4*byte;
    size_of_double     = 8*byte;
    size_of_longdouble = 12*byte;
  }

let get () =  c

let is_char_type_signed = true
(* only defined in gnuc mode *)
let size_of_void = 1*byte
let size_of_char = c.size_of_char
let size_of_ptr = c.size_of_ptr
let size_of_int = c.size_of_int
let size_of_long = c.size_of_long
let size_of_longlong = c.size_of_longlong
let size_of_double = c.size_of_double
let size_of_float = c.size_of_float
let size_of_longdouble = c.size_of_longdouble
let size_of_short = c.size_of_short
let size_of_byte = c.size_of_byte
let max_array_length = c.max_array_length
