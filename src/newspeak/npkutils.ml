open Cil
open Cilutils
open Npkcontext

let c_suffix = ".c"
let npko_suffix = ".no"

module String_set = 
  Set.Make (struct type t = string let compare = Pervasives.compare end)

module Int_set = 
  Set.Make (struct type t = int let compare = Pervasives.compare end)



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
    | _ -> error ("Npkutils.translate_arith_binop")

let translate_float_binop sz o =
  match o with
    | PlusA -> Newspeak.PlusF sz
    | MinusA -> Newspeak.MinusF sz
    | Mult -> Newspeak.MultF sz
    | Div -> Newspeak.DivF sz
    | _ -> error ("Npkutils.translate_float_binop")

let translate_logical_binop t o =
  match o with
    | BOr -> Newspeak.BOr (Newspeak.domain_of_typ t)
    | BAnd -> Newspeak.BAnd (Newspeak.domain_of_typ t)
    | BXor -> Newspeak.BXor (Newspeak.domain_of_typ t)
    | Shiftlt -> Newspeak.Shiftlt
    | Shiftrt -> Newspeak.Shiftrt
    | _ -> error ("Npkutils.translate_arith_binop")


let translate_typ t =
  let rec translate_typ_aux t =
    match t with
        TInt (ISChar, _) | TInt (IChar, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Signed, char_size))
      | TInt (IUChar, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Unsigned, char_size))
      | TInt (IShort, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Signed, short_size))
      | TInt (IUShort, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Unsigned, short_size))
      | TInt (IInt, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Signed, int_size))
      | TInt (IUInt, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Unsigned, int_size))
      | TInt (ILong, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Signed, long_size))
      | TInt (IULong, _) -> Newspeak.Scalar (Newspeak.Int (Newspeak.Unsigned, long_size))

      | TEnum _ -> translate_typ_aux intType
      | TNamed (info, _) -> translate_typ_aux info.ttype

      | TPtr (TFun _, _) -> Newspeak.Scalar Newspeak.FunPtr
      | TPtr (_, _) -> Newspeak.Scalar Newspeak.Ptr
          (* We don't have to check that the type pointed is handled
	     If the pointer is not dereferenced, we don't care, and if
	     it is dereferenced, we will translate the pointed type *)

      | TArray (t, l, _) ->
          let typ = translate_typ_aux t in
          let len = lenOfArray l in
            Newspeak.Array (typ, len)

      | TComp (info, _) ->
          let descr = List.map (translate_field t) info.cfields in
          let sz = size_of t in
            Newspeak.Region (descr, sz)

      | TBuiltin_va_list _ -> error ("Npkutils.translate_typ: "
                                     ^" variable list of arguments not handled yet")
      
      | TFloat (FFloat, _) -> Newspeak.Scalar (Newspeak.Float float_size)

      | TFloat (FDouble, _) -> Newspeak.Scalar (Newspeak.Float double_size)

      | TInt _ | TVoid _ | TFloat _ | TFun _ ->
          error ("Npkutils.translate_typ: the type "
                 ^(string_of_type t)^" is not handled yet")

  and translate_field t f =
    let offset = offset_of t (Field(f, NoOffset)) in
    let typ = translate_typ_aux f.ftype in
      (offset, typ)
  in

    try
      translate_typ_aux t
    with Cil.LenOfArray -> error ("LenOfArray exception on "^string_of_type t)


let translate_rel_binop t1 t2 o =
  let t = 
    match (translate_typ t1, translate_typ t2) with
	(Newspeak.Scalar t1, Newspeak.Scalar t2) when t1 = t2 -> t1
      | _ -> 
	  error ("Npkutils.translate_rel_binop: "
		 ^"incompatible types for comparison")
  in
    match o with
      | Gt -> Newspeak.Gt t
      | Eq -> Newspeak.Eq t
      | _ -> error ("Npkutils.translate_rel_binop")


let compare_typs t1 t2 =
  let rec compare_typs_aux t1 t2 =
    match t1, t2 with
        TInt (i1, _), TInt (i2, _) -> i1 = i2
      | TFloat (f1, _), TFloat (f2, _) -> f1 = f2
      | TEnum (e1, _), TEnum (e2, _) -> compare_enum e1.eitems e2.eitems
      | TNamed (info, _), t
      | t, TNamed (info, _) -> compare_typs_aux info.ttype t
      | TPtr (TFun _, _), TPtr (TFun _, _) -> true
      | TPtr (_, _), TPtr (_, _) -> true

      | TArray (st1, None, _), TArray (st2, _, _)
      | TArray (st1, _, _), TArray (st2, None, _) ->
	  compare_typs_aux st1 st2

      | TArray (st1, l1, _), TArray (st2, l2, _) ->
          let len1 = lenOfArray l1 in
          let len2 = lenOfArray l2 in
	    compare_typs_aux st1 st2 && len1 = len2

      | TComp (info1, _), TComp (info2, _) ->
	  compare_fields t1 t2 info1.cfields info2.cfields

      | _, _ -> false

  and compare_fields t1 t2 l1 l2 =
    match l1, l2 with
	[], [] -> true
      | f1::r1, f2::r2 ->
	  let offset1 = offset_of t1 (Field(f1, NoOffset)) in
	  let offset2 = offset_of t2 (Field(f2, NoOffset)) in
	    compare_typs_aux f1.ftype f2.ftype &&
	      offset1 = offset2 &&
	      compare_fields t1 t2 r1 r2
      | _ -> false

  and compare_enum e1 e2 =
    match e1, e2 with
      | [], [] -> true
      | (s1, ex1, _)::r1, (s2, ex2, _)::r2 ->
	  s1 = s2 && ex1 = ex2 && compare_enum r1 r2
      | _ -> false

  in
    compare_typs_aux t1 t2


let isPtr e =
  match translate_typ (typeOf e) with
    | Newspeak.Scalar Newspeak.Ptr
    | Newspeak.Scalar Newspeak.FunPtr -> true
    | _ -> false
