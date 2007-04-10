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




let translate_loc loc =
  (loc.file, loc.line, loc.byte)

let update_loc loc =
  cur_loc := translate_loc loc

let get_cur_file () =
  let (file, _, _) = !cur_loc in file


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

(* TODO: look at all callers to translate_typ to remove LenOfArray exception
   catch *)
let translate_typ t =
  let rec translate_typ_aux t =
    match t with
        TInt (ISChar, _) | TInt (IChar, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Signed, char_size))
      | TInt (IUChar, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Unsigned, char_size))
      | TInt (IShort, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Signed, short_size))
      | TInt (IUShort, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Unsigned, short_size))
      | TInt (IInt, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Signed, int_size))
      | TInt (IUInt, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Unsigned, int_size))
      | TInt (ILong, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Signed, long_size))
      | TInt (IULong, _) -> 
	  Npkil.Scalar (Newspeak.Int (Newspeak.Unsigned, long_size))

      | TEnum _ -> translate_typ_aux intType
      | TNamed (info, _) -> translate_typ_aux info.ttype

      | TPtr (TFun _, _) -> Npkil.Scalar Newspeak.FunPtr
      | TPtr (_, _) -> Npkil.Scalar Newspeak.Ptr
          (* We don't have to check that the type pointed is handled
	     If the pointer is not dereferenced, we don't care, and if
	     it is dereferenced, we will translate the pointed type *)

      | TArray (t, l, _) ->
          let typ = translate_typ_aux t in
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
      
      | TFloat (FFloat, _) -> Npkil.Scalar (Newspeak.Float float_size)

      | TFloat (FDouble, _) -> Npkil.Scalar (Newspeak.Float double_size)

      | TInt _ | TVoid _ | TFloat _ | TFun _ ->
          error "Npkutils.translate_typ"
	    ("the type "^(string_of_type t)^" is not handled yet")

  and translate_field t f =
    let offset = offset_of t (Field(f, NoOffset)) in
    let typ = translate_typ_aux f.ftype in
      (offset, typ)
  in

(* TODO: Check this *)
(*    try *)
      translate_typ_aux t
(*    with Cil.LenOfArray ->
      error "Npkutils.translate_typ" 
	("LenOfArray exception on "^string_of_type t) *)


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
