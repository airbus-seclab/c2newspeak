(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007-2010  Charles Hymans, Etienne Millon, Sarah Zennou
  
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
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Etienne Millon
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: etienne.millon@eads.net
  
  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah.zennou@eads.net
  
*)

type t = Cstr of string * string
       | Return
       | Value_of of string
       | Misc of string
       | Goto_label of string
       | Ada_operator of string

let to_string id x =
  let sep = "!" in
  let id_str = string_of_int id in
    match x with
      | Cstr (fname, s) -> "cstr" ^ sep ^ id_str ^ sep ^ (fname ^ "." ^ s)
      | Return -> "!return"
      | Value_of s -> "value_of" ^ sep ^ id_str ^ sep ^ s
      | Misc desc -> "tmp_" ^ desc ^ sep ^ id_str
      | Goto_label lbl -> "goto" ^ sep ^ lbl
      | Ada_operator s ->
          let operators =
            [ "and" ; "or" ; "xor" ; "=" ; "/="  ; "<"
            ; "<="  ; ">"  ; ">="  ; "+" ; "-"   ; "*"
            ; "/"   ; "mod" ; "rem" ; "**"
            ]
          in
          if List.mem s operators then
            sep ^ "op" ^ s
          else invalid_arg ("Temps.to_string : ada operator '" ^ s ^ "'")

