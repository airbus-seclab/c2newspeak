/*
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
*/

#define __builtin_va_list             char*
#define __builtin_va_start(v,p)       v = __builtin_newspeak_va_arg;
#define __builtin_va_arg(v,t)         *((*((t**)(&v)))++)
#define __builtin_va_end(v)           
#define __builtin_va_copy(dest, src)  dest = src

/* note:
   __builtin_va_arg(v,t) could be alternatively defined as 
   *(((t*)v)++)
   but this is deprecated and not standard C
 */

//#define __builtin_offsetof(type, field) ((unsigned int)(&((type *)0)->field))

#define __builtin_expect(e, c)     e
