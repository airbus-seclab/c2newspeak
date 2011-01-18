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

(* sizes in number of bits *)
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

val get: unit -> t

val is_char_type_signed: bool
(* only defined in gnuc mode *)
val size_of_void: int
val size_of_char: int
val size_of_ptr: int
val size_of_int: int
val max_sizeof: int
val size_of_long: int
val size_of_longlong: int
val size_of_double: int
val size_of_float: int
val size_of_longdouble: int
val size_of_short: int
val size_of_byte: int
val max_array_length: int
