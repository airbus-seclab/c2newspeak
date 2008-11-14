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

open Parser

let token_tbl = Hashtbl.create 50

let _ = 
  Hashtbl.add token_tbl "__extension__" EXTENSION;
  (* prevent warnings when compiling in -pedantic *)

  Hashtbl.add token_tbl "__attribute__" ATTRIBUTE;  
  Hashtbl.add token_tbl "__format__" FORMAT;
  Hashtbl.add token_tbl "__restrict" RESTRICT;
  Hashtbl.add token_tbl "__format_arg__" FORMAT_ARG;
  Hashtbl.add token_tbl "__printf__" PRINTF;
  Hashtbl.add token_tbl "__scanf__" SCANF;
  Hashtbl.add token_tbl "__builtin_va_list" VA_LIST;
  Hashtbl.add token_tbl "__cdecl__" CDECL_ATTR;
  Hashtbl.add token_tbl "__gnu_inline__" GNU_INLINE;
  Hashtbl.add token_tbl "__inline__" INLINE;
  Hashtbl.add token_tbl "__inline" INLINE;
  Hashtbl.add token_tbl "__always_inline__" ALWAYS_INLINE;
  Hashtbl.add token_tbl "noreturn" NORETURN;
  Hashtbl.add token_tbl "__noreturn__" NORETURN;
  Hashtbl.add token_tbl "dllimport" DLLIMPORT;
  Hashtbl.add token_tbl "__asm__" ASM;
  Hashtbl.add token_tbl "__cdecl" CDECL;
  Hashtbl.add token_tbl "__nothrow__" NOTHROW;
  (* tells the compiler the function does not throw an exception *)

  Hashtbl.add token_tbl "__pure__" PURE;
  (* tells the compiler the function has no side-effects other than the 
     return value which depends on the arguments and globals *)

  Hashtbl.add token_tbl "__const" CONST;
  Hashtbl.add token_tbl "__const__" CONST;
  (* for function slightly more strict than pure, since const functions
     are assumed not to read global variables *)

  Hashtbl.add token_tbl "__nonnull__" NONNULL;
  (* tells the compiler the argument should always be a non-null pointer *)

  Hashtbl.add token_tbl "__deprecated__" DEPRECATED;
  (* generates warnings when the function is used *)

  Hashtbl.add token_tbl "__malloc__" MALLOC;
  Hashtbl.add token_tbl "__builtin_constant_p" BUILTIN_CONSTANT_P;
  Hashtbl.add token_tbl "__mode__" MODE;
  Hashtbl.add token_tbl "__QI__" QI;
  Hashtbl.add token_tbl "__HI__" HI;
  Hashtbl.add token_tbl "__SI__" SI;
  Hashtbl.add token_tbl "__word__" SI;
  Hashtbl.add token_tbl "__DI__" DI;
  Hashtbl.add token_tbl "__warn_unused_result__" WARN_UNUSED_RESULT;
  Hashtbl.add token_tbl "__packed__" PACKED;
  Hashtbl.add token_tbl "__PRETTY_FUNCTION__" FUNNAME


let find_token str =
  Hashtbl.find token_tbl str

let is_gnuc_token str = Hashtbl.mem token_tbl str

let builtins = 
  List.fold_left ( ^ ) "" 
    ["extern char *__builtin_strchr(char *str, char c);";
     "extern int __builtin_strcmp(char *str1, char *str2);";
     "extern char *__builtin_strncat(char *dst, char *src, unsigned int sz);";
     "extern long __builtin_expect(long exp, long val);";
    ]
