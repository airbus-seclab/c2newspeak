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
open Cilutils
open Npkcontext

module Int_set = 
  Set.Make (struct type t = int let compare = Pervasives.compare end)


(*----------------------------*)
(* Useful, non exported stuff *)
(*----------------------------*)

(* Counter are always incremented by incr*)
let incr cnt = 
  if !cnt = max_int
  then error "Npkutils.incr" "too many objects";
  incr cnt;
  !cnt

let translate_arith_binop o =
  match o with
    | PlusA -> Newspeak.PlusI
    | MinusA -> Newspeak.MinusI
    | Mult -> Newspeak.MultI
    | Div -> Newspeak.DivI
    | Mod -> Newspeak.Mod
    | _ -> error "Npkutils.translate_arith_binop" "unexpected operator"

let translate_float_binop sz o =
  match o with
    | PlusA -> Newspeak.PlusF sz
    | MinusA -> Newspeak.MinusF sz
    | Mult -> Newspeak.MultF sz
    | Div -> Newspeak.DivF sz
    | _ -> error "Npkutils.translate_float_binop" "unexpected operator"

let translate_logical_binop t o =
  match o with
    | BOr -> Newspeak.BOr (Newspeak.domain_of_typ t)
    | BAnd -> Newspeak.BAnd (Newspeak.domain_of_typ t)
    | BXor -> Newspeak.BXor (Newspeak.domain_of_typ t)
    | Shiftlt -> Newspeak.Shiftlt
    | Shiftrt -> Newspeak.Shiftrt
    | _ -> error "Npkutils.translate_arith_binop" "unexpected operator"


let cache = Hashtbl.create 100

let translate_ikind k =
  match k with
      ISChar | IChar -> (Newspeak.Signed, Config.size_of_char)
    | IUChar -> (Newspeak.Unsigned, Config.size_of_char)
    | IShort -> (Newspeak.Signed, Config.size_of_short)
    | IUShort -> (Newspeak.Unsigned, Config.size_of_short)
    | IInt -> (Newspeak.Signed, Config.size_of_int)
    | IUInt -> (Newspeak.Unsigned, Config.size_of_int)
    | ILong -> (Newspeak.Signed, Config.size_of_long)
    | IULong -> (Newspeak.Unsigned, Config.size_of_long)
    | ILongLong -> (Newspeak.Signed, Config.size_of_longlong)
    | IULongLong -> (Newspeak.Unsigned, Config.size_of_longlong)

(* TODO: look at all callers to translate_typ to remove LenOfArray exception
   catch *)
let rec translate_typ t =
  try Hashtbl.find cache t
  with Not_found -> 
    let t' =
      match t with
	  TInt (k, _) -> 
	    let k = translate_ikind k in
	      Npkil.Scalar (Newspeak.Int k)

	| TEnum _ -> translate_typ intType
	| TNamed (info, _) -> translate_typ info.ttype
	    
	| TPtr (TFun _, _) -> Npkil.Scalar Newspeak.FunPtr
	| TPtr (_, _) -> Npkil.Scalar Newspeak.Ptr
            (* We don't have to check that the type pointed is handled
	       If the pointer is not dereferenced, we don't care, and if
	       it is dereferenced, we will translate the pointed type *)
	    
	| TArray (t, l, _) ->
            let typ = translate_typ t in
            let len = 
	      try Some (lenOfArray l) 
	      with LenOfArray -> None
	    in
              Npkil.Array (typ, len)
		
	| TComp (info, _) ->
            let descr = List.map (translate_field t) info.cfields in
            let sz = size_of t in
              Npkil.Region (descr, sz)
		
	| TBuiltin_va_list _ ->
	    error "Npkutils.translate_typ" 
              "variable list of arguments not handled yet"
	      
	| TFloat (FFloat, _) -> 
	    Npkil.Scalar (Newspeak.Float Config.size_of_float)
	    
	| TFloat (FDouble, _) -> 
	    Npkil.Scalar (Newspeak.Float Config.size_of_double)

	| TFloat (FLongDouble, _) -> 
	    Npkil.Scalar (Newspeak.Float Config.size_of_longdouble)
	    
	| TVoid _ | TFun _ ->
            error "Npkutils.translate_typ"
	      ("the type "^(string_of_type t)^" is not handled yet")
    in
      Hashtbl.add cache t t';
      t'

and translate_field t f =
  let offset = Cilutils.offset_of t (Field(f, NoOffset)) in
  let typ = translate_typ f.ftype in
    (offset, typ)

let translate_rel_binop t1 t2 o =
  let t = 
    match (translate_typ t1, translate_typ t2) with
	(Npkil.Scalar t1, Npkil.Scalar t2) when t1 = t2 -> t1
      | _ -> 
	  error "Npkutils.translate_rel_binop"
	    "incompatible types for comparison"
  in
    match o with
      | Gt -> Newspeak.Gt t
      | Eq -> Newspeak.Eq t
      | _ ->
	  error "Npkutils.translate_rel_binop"
	    "unexpected operator"

let isPtr e =
  match translate_typ (typeOf e) with
    | Npkil.Scalar Newspeak.Ptr
    | Npkil.Scalar Newspeak.FunPtr -> true
    | _ -> false

let translate_ftyp (args, ret) =
  let translate_ret t =
    match t with
	TVoid _ -> None
      | t -> Some (translate_typ t)
  in
  let translate_arg (x, t, _) = (x, translate_typ t) in
  let args =
    match args with
	None -> None
      | Some args -> Some (List.map translate_arg args)
  in
    (args, translate_ret ret)

(*
    match x with
	TFun (ret, args, _, _) -> 
      | _ -> 
	  error "Npkutils.translate_ftyp" 
	    ("invalid type '"^(Cilutils.string_of_type x)^"'") 
*)
let translate_loc loc = (loc.file, loc.line, loc.byte) 
