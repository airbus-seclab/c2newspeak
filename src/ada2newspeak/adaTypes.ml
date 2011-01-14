(*
  Ada2Newspeak: compiles Ada code into Newspeak. Newspeak is a minimal 
  language well-suited for static analysis.
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

  Etienne Millon
  email: etienne.millon AT gmail . com

*)

(*********************************
 * Module definitions/inclusions *
 *********************************)

module A = AdaSyntax

let (%+) = Newspeak.Nat.add
let (%-) = Newspeak.Nat.sub


(********************
 * Type definitions *
 ********************)

type data_t =
  | IntVal   of Newspeak.Nat.t
  | FloatVal of float
  | BoolVal  of bool

let data_compare x y =
  match (x, y) with
    |   IntVal a,   IntVal b -> Newspeak.Nat.compare a b
    |  BoolVal a,  BoolVal b -> Pervasives.compare a b
    | FloatVal a, FloatVal b -> Pervasives.compare a b
    | _ ->
        Npkcontext.report_error "Ada_types.data_eq"
          "Incompatible types in '=' (data_t)"

(* "subtype" in RM *)
type t = {
  base         : base_t;
  range        : AdaSyntax.contrainte option;
}

(* "type" in RM *)
and base_t = {
  trait   : trait_t;
  uid     : int;     (* Used to make derived types look different *)
}

and range =
  Newspeak.Nat.t * Newspeak.Nat.t (* min <= max *)

(* type for "traits" (type of types...) *)
and trait_t =
  | Unknown     of string            (* Reason *)
  | Univ_int
  | Univ_real
  | Signed      of range
  | Float       of Newspeak.Nat.t    (* Digits           *)
  | Array       of t*t list          (* Component-index  *)
  | Record      of (string * t) list (* Fields           *)
  | Enumeration of (string*int) list (* Name-index       *)
  | Access      of t

(******************
 * Pretty-printer *
 ******************)

let rec print t =
  let p_hash t  = Printf.sprintf "%08x" (Hashtbl.hash t) in
  let p_range = function
    | None      -> "<unlimited>"
    | Some (A.IntegerRangeConstraint (a,b)) ->  "I["
                                               ^ Newspeak.Nat.to_string a
                                               ^ ";"
                                               ^ Newspeak.Nat.to_string b
                                               ^ "]"
    | Some (  A.FloatRangeConstraint (a,b)) ->  "F["
                                               ^ string_of_float a
                                               ^ ";"
                                               ^ string_of_float b
                                               ^ "]"
  in
  let p_trait = function
    | Unknown _ -> "Unknown"
    | Signed  (a,b) -> "Signed "^p_range (Some (A.IntegerRangeConstraint (a,b)))
    | Univ_int    -> "Universal_integer"
    | Univ_real   -> "Universal_real"
    | Float   d -> "Float "  ^Newspeak.Nat.to_string d
    | Enumeration v -> "Enum (length = "^string_of_int (List.length v)^")"^(String.concat "/" (List.map	(fun (x,v) ->  (x^" = "^(string_of_int v))) v))
    | Array (c,i) -> "Array {{ component = "
                  ^print c^"; index = "
                  ^String.concat "," (List.map print i)
                  ^"}}"
    | Record flds -> "Record {"^String.concat ", " (List.map fst flds)^"}"
    | Access t -> "Access "^p_hash t
  in
   "{"
  ^"H="^p_hash t
  ^", "
  ^(if t.base.uid = 0 then "" else "U="^string_of_int t.base.uid)
  ^", trait = "^p_trait t.base.trait
  ^", range = "^p_range t.range
  ^"}"

let print_data = function
  | IntVal   i -> "IV ("^Newspeak.Nat.to_string i^")"
  | FloatVal s -> "FV ("^string_of_float        s^")"
  | BoolVal  b -> "BV ("^string_of_bool         b^")"

(**********
 * Ranges *
 **********)

let sizeof = function
  | A.IntegerRangeConstraint (min,max) -> max %- min %+ Newspeak.Nat.one
  |   A.FloatRangeConstraint _ -> invalid_arg "sizeof"

(*****************
 * Symbol tables *
 *****************)

(*
 * Create a new type uid.
 * /!\ Side-effects.
 *)
let uid = object
  val mutable count = 0;
  method gen =
    count <- count + 1;
    count
end

let new_unknown reason =
  { base = { trait = Unknown reason
           ; uid = uid#gen
           }
  ; range = None
  }

let universal_integer =
  { base = { trait = Univ_int
           ; uid = uid#gen
           }
  ; range=None
  }

let universal_real =
  { base = { trait = Univ_real
           ; uid   = uid#gen
           }
  ; range=None
  }

let new_enumerated values =
    let rec with_indices vals offset = match vals with
    |   []  -> []
    | n::tl -> (n,offset)::with_indices tl (offset+1)
    in
    let ivalues = with_indices values 0 in
    { base = { trait = Enumeration ivalues
             ; uid = uid#gen
             }
    ; range = None
    }

let new_derived old =
  {
    old with
    base = { old.base with uid = uid#gen };
  }

let new_constr parent r =
  {
    base = parent.base;
    range = Some r ;
  }


let new_enum parent en =
  {
    base = {
      trait = Enumeration en;
      uid = parent.base.uid;
    };
    range = parent.range ;
  }

let new_range c =
  let r = match c with
  | A.IntegerRangeConstraint (a,b) -> (a,b)
  | A.FloatRangeConstraint _ -> invalid_arg "new_range"
  in
  { base = { trait = Signed r
           ; uid = uid#gen
           }
  ; range = None
  }

let new_float digits =
    {
      base = { trait = Float digits
             ; uid = uid#gen
             }
    ; range = None
    }

let new_array ~component ~index =
    {
      base = { trait = Array (component,index)
             ; uid = uid#gen
             }
    ; range = None
    }

let new_record fields =
  { base = { trait = Record fields
           ; uid = uid#gen
           }
  ; range = None
  }

let new_access t =
  { base = { trait = Access t
           ; uid = uid#gen
           }
  ; range = None
  }

let extract_array_types t =
  match t.base.trait with
  | Array (c, i) -> (c,i)
  | _            -> Npkcontext.report_error "extract_array_types"
                      "assertion (is_array) failed"

let extract_array_range t =
  match (extract_array_types t) with
      (_, [i]) -> i
    | _      -> Npkcontext.report_error "extract_array_range"
        "assertion unique indexes failed"

let get_reason t =
  match t.base.trait with
  | Unknown r -> r
  | _ -> Npkcontext.report_error "get_reason"
          "assertion (is_unknown) failed"


(**************************
 * Representation clauses *
 **************************)
let represtbl : (t, (data_t * data_t) list) Hashtbl.t = Hashtbl.create 0

let extract_symbols t =
  match t.base.trait with
    | Enumeration v  -> Some v
	(* WG Some v
	begin
	  try 
	   let mapping = Hashtbl.find represtbl t in
	   let lg = List.length mapping in 
	   if (compare lg (List.length v) <> 0) then
	   Npkcontext.report_error "extract symbols"
	   "assertion (same length) failed"
	   ;
	   Some ( List.map 
	   ( fun (x,y) -> 
	   match y with 
	   IntVal nat -> 
	   (x, Newspeak.Nat.to_int nat)
	   | _ ->   
	   Npkcontext.report_error 
	   "extract  symbols"
	   "assertion (not intval) failed"
	   ) 
	   (List.map2 (fun (s,_) (n, _) -> (s,n)) v mapping)
	   )
	   with 
	   Not_found -> Some v
	   end*)
    | _             -> None
	
let extract_access_type t =
  match t.base.trait with
  | Access te -> te
  | _ -> Npkcontext.report_error "extract_access_type"
           "This type is not an access type, it cannot be dereferenced"



let is_increasing l =
  match l with
  |  []  -> invalid_arg "is_increasing"
  | h::t -> fst (List.fold_left (fun (r,pred) e ->
      if r then (Newspeak.Nat.compare e pred > 0,e)
      else false, Newspeak.Nat.zero
  ) (true, h) t)

let handle_enum_repr_clause t l =
  let values_expected =
  begin match t.base.trait with
    | Enumeration v -> List.map (fun (_,x) -> Newspeak.Nat.of_int x) v
    | _ -> Npkcontext.report_error "handle_enum_repr_clause"
                           "This type is not an enumeration"
  end 
  in
    if Hashtbl.mem represtbl t then
      Npkcontext.report_error "handle_enum_repr_clause"
      "A representation clause has already been given for this type";
    
    let (values_got, values_int) = List.split l in
      if not (is_increasing values_int) then
	Npkcontext.report_error "handle_enum_repr_clause"
	  "In representation clause, values should be ordered";
      
      if (values_got <> values_expected) then
	Npkcontext.report_error "handle_enum_repr_clause"
	  "In representation clause, some litterals are missing";
      
      let mapping = List.map (fun (x,y) -> (IntVal x, IntVal y)) l in
	(*Building a new type with the use representation values*)
      let new_en = 
	begin match t.base.trait with
	  | Enumeration v -> List.map2 
	      (fun (x,_) got -> x, Newspeak.Nat.to_int got)
		v  values_int 
	  | _ -> Npkcontext.report_error "handle_enum_repr_clause"
              "This type is not an enumeration"
	end 
      in
      let upd_type =  new_enum t new_en in
	Hashtbl.add represtbl upd_type mapping;
	upd_type
	  (* FIXME check if not frozen *)
	  
let  get_enum_litt_value _ x = x
  (*
    try let l = Hashtbl.find represtbl t in
    List.assoc x l
    with Not_found ->  
    x
  *)

(*****************
 * Builtin types *
 *****************)

let system_address = new_derived (universal_integer)

let integer_first = Newspeak.Nat.of_string "-2147483648"
let integer_last  = Newspeak.Nat.of_string  "2147483647"


let float_first = -3.40282E+38
let float_last  = 3.40282E+38


let integer = new_range (A.IntegerRangeConstraint(integer_first, integer_last))

let boolean = new_enumerated ["true" ; "false"]

let std_float = new_float (Newspeak.Nat.of_int 6)

(*
 * This declaration is special because we don't want to make the character
 * litterals (named __char_nnn) public.
 * TODO : make them public with a reserved name like __ada2npk_char_nnn and at
 * parse time change 'x' to the correct variable lookup.
 *)
let character =
  let rec all_bytes_from x =
    if x<0 then []
    else x::all_bytes_from (x-1)
    in
  new_enumerated (List.rev_map
                  (fun x -> "__char_"^(string_of_int x))
                  (all_bytes_from 255)
                 )

(****************
 * Traits tests *
 ****************)

let is_boolean typ =
  typ.base == boolean.base

let is_integer typ =
  match typ.base.trait with
  | Signed      _ -> true
  | Univ_int      -> true
  | _             -> false

let is_discrete typ =
  match typ.base.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Enumeration _ -> true
  | Float       _ -> false
  | Signed      _ -> true
  | Univ_int      -> true
  | Univ_real     -> false
  | Access      _ -> false

let is_numeric typ =
  match typ.base.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Enumeration _ -> false
  | Float       _ -> true
  | Signed      _ -> true
  | Univ_int      -> true
  | Univ_real     -> true
  | Access      _ -> false

let is_scalar typ =
  match typ.base.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Float       _ -> true
  | Enumeration _ -> true
  | Signed      _ -> true
  | Univ_int      -> true
  | Univ_real     -> true
  | Access      _ -> false

let is_float typ =
  match typ.base.trait with
  | Float       _ -> true
  | Univ_real     -> true
  | _             -> false

let is_array typ =
  match typ.base.trait with
  | Array _ -> true
  | _       -> false

let is_record typ =
  match typ.base.trait with
  | Record _ -> true
  | _       -> false

let is_unknown typ =
  match typ.base.trait with
  | Unknown     _ -> true
  | _             -> false

(* Number of values in a type *)
let length_of typ = match (typ.base.trait, typ.range) with
| Signed       _  , Some r -> sizeof r
| Signed       r  , None -> sizeof (A.IntegerRangeConstraint r)
| Univ_int        , Some r -> sizeof r
| Enumeration vals, _      -> Newspeak.Nat.of_int (List.length vals)
| Array        _  , _
| Record       _  , _
| Float        _  , _
| Unknown      _  , _
(*| Signed       _  , _ *)
| Access       _  , _
| Univ_int        , _
| Univ_real       , _
    -> Npkcontext.report_error "length_of" "Type with no size"

let (<=%) a b =
  Newspeak.Nat.compare a b <= 0

let extrema l =
  match l with
  |  [] -> invalid_arg "extrema"
  | h::t ->
  List.fold_left (fun (min, max) x ->
    if      data_compare x min < 0 then (x  , max)
    else if data_compare max x < 0 then (min, x  )
    else                                (min, max)
  ) (h,h) t

let compute_constr t = 
  match (t.base.trait, t.range) with
    | Signed (a,b), None -> Some (A.IntegerRangeConstraint(a, b))
    | Signed (a,b), Some (A.IntegerRangeConstraint(c,d)) ->
        begin
          assert (a <=% c && c <=% d && d <=% b);
          t.range
        end
    | Float _ , Some (A.FloatRangeConstraint _) -> t.range
    | Enumeration v, None
        when not (is_boolean t)->
	begin
            let v' = List.map (fun (_, y) ->
              get_enum_litt_value t (IntVal (Newspeak.Nat.of_int y))
			      ) v in
            let (min, max) = match extrema v' with
            | IntVal x, IntVal y -> (x,y)
            | _ -> invalid_arg "compute_constr"
            in
            Some (A.IntegerRangeConstraint (min, max))
          end

    | Enumeration _, Some (A.IntegerRangeConstraint (min, max))
        when not (is_boolean t)-> begin	
	  (* TO DO check t is not in table clause representing*)
	  (*USELESS as represtbl .... *)
	  try
	    let mapping = Hashtbl.find represtbl t in 
	    let new_min = List.assoc (IntVal min) mapping in 
	    let new_max = List.assoc (IntVal max) mapping in 
	      match (new_min, new_max) with 
		  (IntVal mini, IntVal maxi) ->
		    Some (A.IntegerRangeConstraint (mini, maxi))
		| _ -> invalid_arg "compute_constr represing clause not expected"
	  with Not_found ->	 
	    
            Some (A.IntegerRangeConstraint (min, max))
	end
    | _ -> None

let compute_int_constr t = 
  match (compute_constr t) with
      Some (A.IntegerRangeConstraint(a, b)) -> Some (a,b)
    | _ -> None
	
let all_values typ =
  let rec interval a b =
    if (Newspeak.Nat.compare b a < 0) then []
    else a::(interval (Newspeak.Nat.add_int 1 a) b)
  in
  match (compute_constr typ) with
    | Some (A.IntegerRangeConstraint(a, b)) -> interval a b
    | _ -> invalid_arg "all_values"

(****************
 *  Translator  *
 ****************)

(**
   Minimal integer n such that
       -(2^(n-1)) <= a <= b <= 2^(n-1) - 1
 *)
let minimal_size_signed a b =
  let (i,two_pow_i) = ref 0, ref Newspeak.Nat.one in
  let finished _ =
       (Newspeak.Nat.compare (Newspeak.Nat.neg (!two_pow_i))
                             a  <= 0)
    && (Newspeak.Nat.compare b (Newspeak.Nat.add_int (-1) (!two_pow_i)) <= 0)
  in
  while (not (finished ())) do
    incr i;
    two_pow_i := Newspeak.Nat.mul_int 2 !two_pow_i;
  done;
  !i + 1

(**
   Minimal integer n such that
       0 <= b <= 2^n - 1
 *)
let minimal_size_unsigned b' =
  let b = Newspeak.Nat.to_int b' in
  let (i,two_pow_i) = ref 0, ref 1 in
  let finished _ =
    b <= !two_pow_i - 1
  in
  while (not (finished ())) do
    incr i;
    two_pow_i := 2 * !two_pow_i;
  done;
  !i

let float_size _d =
  (* FIXME *)
  32

let rec translate t = match t.base.trait with
  | Signed (a,b) -> Cir.Scalar
                             (Newspeak.Int
                               ( Newspeak.Signed
                               , minimal_size_signed a b
                               ))
  | Enumeration    v    -> begin
         let (min, max) = match (compute_constr t) with
           | Some (A.IntegerRangeConstraint (x,y))-> (x,y)
           | _ -> ( Newspeak.Nat.zero
                      , Newspeak.Nat.of_int
                        (snd (ListUtils.last v)))
         in
         let ikind =
           if (Newspeak.Nat.compare min
                 Newspeak.Nat.zero < 0) then
             ( Newspeak.Signed
                 , minimal_size_signed min max)
           else
             ( Newspeak.Unsigned
                 , minimal_size_unsigned max)
         in
           Cir.Scalar (Newspeak.Int ikind)
    end
  | Float  d  -> 
      Cir.Scalar (Newspeak.Float (float_size d))
  | Array        (c,is) ->
      begin
        match is with
        | [] -> invalid_arg "translate"
        | i0::i ->
	    List.fold_left (fun (ar:Cir.typ) (ind:t) ->
              Cir.Array ( ar
                        , Some (Newspeak.Nat.to_int
                                   (length_of ind)
                               )
              )
            ) (translate c) (i0::i);
      end
  | Record        flds  ->
      let build_offset (cflds, start_off) (id, st) =
        let ctyp = translate st in
          (id, (start_off, ctyp))::cflds, (start_off + Cir.size_of_typ ctyp)
      in
        Cir.Struct (List.fold_left build_offset ([], 0) flds)
  | Access         _    -> Cir.Scalar Newspeak.Ptr
  | Unknown _ -> Npkcontext.report_error "Ada_types.translate"
               "Type of unknown trait remaining at translate time"
  | Univ_int  ->
      (*
       * This is hacky : actually, typechecking should get rid of this by
       * synthetizing CInts' type from the context, but this could need a
       * two-phase typechecker.
       * Here we only type as Integer.
       *)
      Npkcontext.print_debug "Universal_integer remaining at translate time";
      translate integer
  | Univ_real ->
      (*
       * Same thing : we type constants as Float.
       *)
      Npkcontext.print_debug "Universal_real remaining at translate time";
      translate std_float

let record_field t fld =
  match t.base.trait with
  | Record l -> begin
                  let (off, result) = List.fold_left
                    (fun (off,found) (field,tf) ->
                       if (found = None) then
			 begin
                           if field = fld then
                             (off, Some tf)
                           else
                             (off + Cir.size_of_typ (translate tf), None)
			 end
                       else (* already found *)
			 (off, found)
                    ) (0, None) l in
                  match result with
                  | Some r -> (off,r)
                  | None   -> Npkcontext.report_error "record_field"
                                ("No such field : '"^fld^"'")
                end
  | _ -> Npkcontext.report_error "record_field"
          "assertion (is_record) failed"

let rec attr_get typ attr =
  let const_nat x =
    A.CInt x
  in
  let const_int x =
    const_nat (Newspeak.Nat.of_int x)
  in
  match (typ.base.trait, attr) with
    | Signed      _ ,"first" ->
        begin
          match compute_constr typ with
          | Some (A.IntegerRangeConstraint(a,_)) -> A.CInt a, typ
          | _ -> Npkcontext.report_error "attr_get"
                   "This type does not have a 'first attribute"
        end
    | Signed      _ ,"last" ->
        begin
          match compute_constr typ with
          | Some (A.IntegerRangeConstraint(_,b)) -> A.CInt b, typ
          | _ -> Npkcontext.report_error "attr_get"
                   "This type does not have a 'first attribute"
        end
    | Enumeration v, "first" -> const_int (snd (List.hd v)), typ
    | Enumeration v, "last"  -> const_int (snd (ListUtils.last v)), typ
    | Array (_,inds) , ("first"|"last"|"length")  ->
	begin
          let ind = match inds with
            | []  -> invalid_arg "attr_get"
            | [x] -> x
            | _ -> Npkcontext.report_error "attr_get"
                "Attribute for matrix types"
          in
            if attr = "length" then
              A.CInt (length_of ind), universal_integer
          else
            attr_get ind attr
        end
    | Float digits , "digits" -> const_nat digits, universal_integer
    | Float _ , "safe_small"  -> A.CFloat (min_float), universal_real
    | Float _ , "safe_large"  -> A.CFloat (max_float), universal_real
    | Float _ , "first"       -> 
        begin
          match typ.range with
          | Some (A.FloatRangeConstraint (x,_)) -> A.CFloat x, universal_real
          | None -> ( A.CFloat float_first, typ)
	  | _ -> Npkcontext.report_error "attr_get"
                     "Unconstrained subtype has no 'first attribute"
        end
    | Float _ , "last"       ->
	begin
          match typ.range with
          | Some (A.FloatRangeConstraint (_,y)) -> A.CFloat y, universal_real
          | None -> (A.CFloat float_last, typ)
	  | _ ->
	      Npkcontext.report_error "attr_get"
                "Unconstrained subtype has no 'last attribute"
	   
        end
    | _             , "size"  -> let sz = Cir.size_of_typ (translate typ) in
                                 const_int sz, universal_integer
    | Unknown     _ , _
    | Array       _ , _
    | Record      _ , _
    | Float       _ , _
    | Enumeration _ , _
    | Signed      _ , _
    | Access      _ , _
    | Univ_int      , _
    | Univ_real     , _ -> Npkcontext.report_error "attr_get"
                           ("No such attribute : '"^attr^"'")

let belongs min max exp =
    Cir.Unop ( Npkil.Belongs_tmp (min, Npkil.Known (Newspeak.Nat.add_int 1 max))
             , exp
             )

let check_exp t_ctx exp =
  let constr_combine c1 c2 =
    match (c1,c2) with
      | _         , None       -> c1
      | None      , _          -> c2
      |   Some (A.IntegerRangeConstraint(a,b))
          , Some (A.IntegerRangeConstraint(c,d))
            -> if (a <=% c && d <=% b) then None
            else c1
      | _ -> Npkcontext.report_error "check_exp" "Float range constraint found"
  in
  let extract_constr e = 
    match e with
    | Cir.Unop ( Npkil.Belongs_tmp (a, Npkil.Known b_plus_1)
		   , _
               )
      -> Some (A.IntegerRangeConstraint(a, Newspeak.Nat.add_int (-1) b_plus_1))
    | _ -> None
  in
  let constr =
    constr_combine (compute_constr t_ctx)
                   (extract_constr exp)
  in
  match constr with
  | Some (A.IntegerRangeConstraint(a,b)) -> belongs a b exp
  | Some (A.FloatRangeConstraint  (a,b)) ->
      Npkcontext.report_warning "check_exp"
        ( "An interval check should be inserted here ["
        ^ string_of_float a
        ^ ";"
        ^ string_of_float b
        ^ "]");
      exp
  | _ -> exp


let extract_base ti =
  match compute_constr ti with
    | Some (A.IntegerRangeConstraint(a, _)) -> a
    | _           -> Npkcontext.report_error "extract_array_base"
                                             "bad array index type"

let all_record_fields t =
  match t.base.trait with
  | Record l -> List.map fst l
  | _ -> Npkcontext.report_error "all_record_fields"
           "This type is not a record type"

let coerce_types a b =
  match (a.base.trait,b.base.trait) with
  | Signed   _  , Univ_int   -> a
  | Univ_int    , Signed   _ -> b
  | _ -> a

let rec is_compatible one another =
  if (is_unknown one || is_unknown another) then
    Npkcontext.report_warning "is_compatible"
        "testing compatibility against unknown type";
       (one.base = another.base)
    || (match (one.base.trait, another.base.trait) with
        | Signed   _ , Univ_int (*constant return Univ_Int*)
        | Univ_int   , Signed   _
        | Float    _ , Univ_real
        | Univ_real  , Float    _
            -> true
	| Array _ , Array _  -> 
	    one =  another
        | _ -> false
        )
