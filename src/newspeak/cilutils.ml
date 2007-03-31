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
let pointer_size = size_of (TPtr(TInt (IInt, []), []))
let float_size = size_of (TFloat (FFloat, []))
let double_size = size_of (TFloat (FDouble, []))



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

let dump stdout file = dumpFile !cilPrinter stdout "" file



(*---------------*)
(* Miscellaneous *)
(*---------------*)

let stmt_of_stmtkind sk =
  {labels = []; skind = sk; sid = 0; succs = []; preds = []}

let null = CastE (TPtr (TVoid [], []), zero)

