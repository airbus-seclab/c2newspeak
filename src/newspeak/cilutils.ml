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

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)


open Cil


(*---------*)
(* C types *)
(*---------*)

(* TODO: this is dangerous because of silent overflows when the sizeof are 
   really large *)
let size_of t = bitsSizeOf(t) / 8

let rec size_of_subtyp t =
  match t with
    | TNamed (info, _) ->
	size_of_subtyp info.ttype

    | TPtr (t, _) -> size_of t

    | TArray (t, _, _) -> size_of t

    | _	-> invalid_arg ("Cilutils.size_of_subtyp: "
			^"data pointer type expected")


let offset_of typ o = 
  let (from_base, _) = (Cil.bitsOffset typ o) in  
    from_base/8

(* We have to call initCIL before using size_of *)
let _ = initCIL ()

(* For efficiency reasons, use these variables instead of size_of *)
let char_size = size_of (TInt(IChar, []))
let short_size = size_of (TInt(IShort, []))
let int_size = size_of (TInt(IInt, []))
let long_size = size_of (TInt (ILong, []))
(* TODO: 8, is to have max_int unsigned, so put 4, here *)
let long_long_size = 4
let pointer_size = size_of (TPtr(TInt (IInt, []), []))
let float_size = size_of (TFloat (FFloat, []))
let double_size = size_of (TFloat (FDouble, []))
let long_double_size = size_of (TFloat (FLongDouble, []))



(*-------------------*)
(* Display functions *)
(*-------------------*)

let cilPrinter = ref defaultCilPrinter

let setCilPrinter str =
  if str = "plain" then cilPrinter := Cil.plainCilPrinter
  else cilPrinter := Cil.defaultCilPrinter 

 
let string_of_type t =
  Pretty.sprint 100 (printType !cilPrinter () t)

let string_of_global x =
  Pretty.sprint 100 (printGlobal !cilPrinter () x)

let string_of_init i =
  Pretty.sprint 100 (printInit !cilPrinter () i)

let string_of_exp e =
  Pretty.sprint 100 (printExp !cilPrinter () e)

let string_of_lval e =
  Pretty.sprint 100 (printLval !cilPrinter () e)

let string_of_instr x =
  Pretty.sprint 100 (printInstr !cilPrinter () x)

let string_of_attribute a =
  Pretty.sprint 100 (printAttr !cilPrinter () a)

let string_of_cast (t_src, t_dst) e =
  let cast = string_of_exp (CastE (t_dst, e)) in
  let t_src = string_of_type t_src in
  let t_dst = string_of_type t_dst in
    "'"^t_src^"' -> '"^t_dst^"' in '"^cast^"'"

let dump stdout file = dumpFile !cilPrinter stdout "" file



(*---------------*)
(* Miscellaneous *)
(*---------------*)

let stmt_of_stmtkind sk =
  {labels = []; skind = sk; sid = 0; succs = []; preds = []}

let null = CastE (TPtr (TVoid [], []), zero)

