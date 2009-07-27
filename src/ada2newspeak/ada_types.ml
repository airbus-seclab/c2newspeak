(*
  Ada2Newspeak: compiles Ada code into Newspeak. Newspeak is a minimal language
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

  Etienne Millon
  email: etienne.millon AT gmail . com

*)

(*********************************
 * Module definitions/inclusions *
 *********************************)

let (%+) = Newspeak.Nat.add
let (%-) = Newspeak.Nat.sub


(********************
 * Type definitions *
 ********************)

type data_t =
  | IntVal   of Newspeak.Nat.t
  | FloatVal of float
  | BoolVal  of bool

let data_eq x y =
  match (x, y) with
    |   IntVal v1,   IntVal v2 -> v1 = v2
    |  BoolVal v1,  BoolVal v2 -> v1 = v2
    | FloatVal v1, FloatVal v2 -> v1 = v2
    | _ ->
        Npkcontext.report_error "Ada_types.data_eq"
          "Incompatible types in '=' (data_t)"

let data_lt x y =
  match (x, y) with
    |   IntVal v1,   IntVal v2 -> (Newspeak.Nat.compare v1 v2) < 0
    |  BoolVal v1,  BoolVal v2 -> v1 < v2
    | FloatVal v1, FloatVal v2 -> v1 < v2
    | _ ->
        Npkcontext.report_error "Ada_types.data_lt"
          "Incompatible types in '<' (data_t)"

(* "subtype" in RM *)
type t = {
  base  : base_t;
  range : range option
}

(* "type" in RM *)
and base_t = {
  trait   : trait_t;
  uid     : int;      (* Used to make derived types look different *)
}

and range =
  Newspeak.Nat.t*Newspeak.Nat.t (* min <= max *)

(* type for "traits" (type of types...) *)
and trait_t =
  | Unknown     of string            (* Reason *)
  | Univ_int
  | Univ_real
  | Signed      of range
  | Float       of int               (* Digits           *)
  | Array       of t*t               (* Component-index  *)
  | Record      of (string * t) list (* Fields           *)
  | Enumeration of (string*int) list (* Name-index       *)


(* effective type + wrapped data *)
type value = t*data_t

(******************
 * Pretty-printer *
 ******************)

let rec print t =
  let p_hash t  = Printf.sprintf "%08x" (Hashtbl.hash t) in
  let p_range = function
    | None      -> "<unlimited>"
    | Some (a,b) ->  "["
                   ^ Newspeak.Nat.to_string a
                   ^ ";"
                   ^ Newspeak.Nat.to_string b
                   ^ "]"
  in
  let p_trait = function
    | Unknown _ -> "Unknown"
    | Signed  r   -> "Signed "^p_range (Some r)
    | Univ_int    -> "Universal_integer"
    | Univ_real   -> "Universal_real"
    | Float   d -> "Float "  ^string_of_int d
    | Enumeration v -> "Enum (length = "^string_of_int (List.length v)^")"
    | Array (c,i) -> "Array {{ component = "
                  ^print c^"; index = "
                  ^print i
                  ^"}}"
    | Record flds -> "Record {"^String.concat ", " (List.map fst flds)^"}"
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

let sizeof (min,max) =
  max %- min %+ Newspeak.Nat.one

let (@...) x y = (x, y)

let (@..)  x y = ( Newspeak.Nat.of_int x
                 , Newspeak.Nat.of_int y)

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

let base_type x = {
  base = {x.base with uid = 0};
  range = None
}

let mk_unknown reason = {base = {trait = Unknown reason ; uid = 0}; range = None}

let unknown = mk_unknown "<No reason provided>"

let universal_integer = { base = {trait = Univ_int;
                                  uid = 0};
                          range=None}

let universal_real = { base = {trait = Univ_real;
                               uid = 0};
                          range=None}

let new_enumerated values =
    let rec with_indices vals offset = match vals with
    |   []  -> []
    | n::tl -> (n,offset)::with_indices tl (offset+1)
    in
    let ivalues = with_indices values 0 in
    {
      base = {trait = Enumeration ivalues;
              uid = 0
      };
      range = None
    }

let new_derived_base old =
    { old with uid = uid#gen }
 
let new_derived old =
  {
    old with
    base = new_derived_base old.base;
  }

let new_unconstr parent =
  let parent_base = parent.base in
    {
      base = {parent_base with uid = uid#gen};
      range = None
    }

let new_constr parent r =
  let parent_base = parent.base in
    {
      base = parent_base;
      range = Some r
    }

let new_range r =
  { base = { trait = Signed r;
             uid = 0
           };
    range = None
  }

let new_float digits =
    {
      base = {
        trait = Float digits;
        uid = 0
      };
      range = None
    }

let new_array ~component ~index =
    {
      base = {
        trait = Array (component,index);
        uid = 0;
      };
      range = None
    }

let new_record fields =
  { 
      base = {
        trait = Record fields;
        uid = 0;
      };
      range = None
  }

let handle_representation_clause _t _l =
  invalid_arg ("handle_representaion_clause")

let extract_array_types t =
  match t.base.trait with
  | Array (c, i) -> Some (c,i)
  | _            -> None

let get_reason t =
  match t.base.trait with
  | Unknown r -> r
  | _ -> invalid_arg "get_reason"

let extract_symbols t =
  match t.base.trait with
  | Enumeration v -> Some v
  | _             -> None

(*****************
 * Builtin types *
 *****************)

let integer_first = Newspeak.Nat.of_string "-2147483648"
let integer_last  = Newspeak.Nat.of_string  "2147483647"

let integer = new_range (integer_first, integer_last)

let boolean = new_enumerated ["true" ; "false"]

let std_float = new_float 6

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
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Enumeration _ -> false
  | Float       _ -> false
  | Signed      _ -> true
  | Univ_int      -> true
  | Univ_real     -> false

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

let is_float typ =
  match typ.base.trait with
  | Unknown     _ -> false
  | Array       _ -> false
  | Record      _ -> false
  | Float       _ -> true
  | Enumeration _ -> false
  | Signed      _ -> false
  | Univ_int      -> false
  | Univ_real     -> true

let is_unknown typ =
  match typ.base.trait with
  | Unknown     _ -> true
  | Array       _ -> false
  | Record      _ -> false
  | Float       _ -> false
  | Enumeration _ -> false
  | Signed      _ -> false
  | Univ_int      -> false
  | Univ_real     -> false

(* Number of values in a type *)
let length_of typ = match (typ.base.trait, typ.range) with
| Signed       _  , Some r -> sizeof r
| Enumeration vals, _      -> Newspeak.Nat.of_int (List.length vals)
| Array        _  , _
| Record       _  , _
| Float        _  , _
| Unknown      _  , _
| Signed       _  , _
| Univ_int        , _
| Univ_real       , _
    -> Npkcontext.report_error "length_of" "Type with no size"

let rec attr_get typ attr =
  match (typ.base.trait, attr) with
    | Signed   (a,_),"first" -> typ, IntVal a
    | Signed   (_,b),"last"  -> typ, IntVal b
    | Enumeration values, "first" -> typ, IntVal (Newspeak.Nat.of_int (snd
                                                    (List.hd  values)))
    | Enumeration values, "last"  -> typ, IntVal (Newspeak.Nat.of_int (snd
                                                      (List_utils.last values)))
    | Array (_,ind) , ("first"|"last"|"length")  ->
        begin
          if attr="length" then
            universal_integer, IntVal (length_of ind)
          else
            attr_get ind attr
        end
    | Float digits , "digits" -> universal_integer,
                                    IntVal (Newspeak.Nat.of_int digits)
    | Float _ , "safe_small"  -> universal_real, FloatVal (min_float)
    | Float _ , "safe_large"  -> universal_real, FloatVal (max_float)
    | _  , "succ" when is_scalar typ -> failwith "succ"
    | Unknown     _ , _
    | Array       _ , _
    | Record      _ , _
    | Float       _ , _
    | Enumeration _ , _
    | Signed      _ , _
    | Univ_int      , _
    | Univ_real     , _
                 -> raise ( Invalid_argument ("No such attribute : '"^attr^"'"))

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
let minimal_size_unsigned b =
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

let rec translate t =
  let rec translate_trait = function
  | Signed (a,b) -> Cir.Scalar
                             (Newspeak.Int
                               ( Newspeak.Signed
                               , minimal_size_signed a b
                               ))
  | Enumeration    v    -> Cir.Scalar
                            (Newspeak.Int
                              ( Newspeak.Unsigned
                              , minimal_size_unsigned
                                 (snd (List_utils.last v))))
  | Float          d    -> Cir.Scalar (Newspeak.Float (float_size d))
  | Array        (c,i)  -> Cir.Array  ( translate c
                                      , Some (Newspeak.Nat.to_int
                                                        (length_of i)))
  | Record        flds  ->
      let build_offset (cflds, start_off) (id, st) =
        let ctyp = translate st in
          (id, (start_off, ctyp))::cflds, (start_off + Cir.size_of_typ ctyp)
      in
        Cir.Struct (List.fold_left build_offset ([], 0) flds)
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
      translate_trait integer.base.trait
  | Univ_real ->
      (*
       * Same thing : we type constants as Float.
       *)
      Npkcontext.print_debug "Universal_real remaining at translate time";
      translate_trait std_float.base.trait

  in translate_trait t.base.trait

let belongs min max exp =
    Cir.Unop ( Npkil.Belongs_tmp (min, Npkil.Known (Newspeak.Nat.add_int 1 max))
             , exp
             )

let (<=%) a b =
  Newspeak.Nat.compare a b <= 0

let constr_combine c1 c2 =
  match (c1,c2) with
    | _         , None       -> c1
    | None      , _          -> c2
    | Some (a,b), Some (c,d) -> if (a <=% c && d <=% b)
                                  then None
                                  else c1

let extract_constr e = match e with
  | Cir.Unop ( Npkil.Belongs_tmp (a, Npkil.Known b_plus_1)
           , _
           ) -> Some (a, Newspeak.Nat.add_int (-1) b_plus_1)
  | _ -> None

let compute_constr t =
  match (t.base.trait, t.range) with
    | Signed (a,b), None -> Some (a, b)
    | Signed (a,b), Some (c,d) ->
        begin
          assert (a <=% c && c <=% d && d <=% b);
          Some (c, d)
        end
    | _ -> None

let check_exp t_ctx exp =
  let constr = 
    constr_combine (compute_constr t_ctx)
                   (extract_constr exp)
  in
  match constr with
  | None -> exp
  | Some (a,b) -> belongs a b exp


let extract_array_base t =
  match t.base.trait with
  | Array (_, ti) ->
      begin
        match compute_constr ti with
          | None        -> Npkcontext.report_error "extract_array_base"
                                                   "bad array index type"
          | Some (a, _) -> a
      end
  | _             -> invalid_arg "extract_array_base"

(**
 * Subprogram parameters.
 *)

type f_param = { fp_name : string
               ; fp_in   : bool
               ; fp_out  : bool
               ; fp_type : t
               }

let   to_fparam (a,b,c,d) = {  fp_name = a;fp_in = b;fp_out = c;fp_type = d}
let from_fparam     f     = (f.fp_name , f.fp_in , f.fp_out , f.fp_type)

let coerce_types a b =
  match (a.base.trait,b.base.trait) with
  | Signed   _  , Univ_int   -> a
  | Univ_int    , Signed   _ -> b
  | _ -> a

let is_compatible one another =
  if (is_unknown one || is_unknown another) then
    Npkcontext.report_warning "is_compatible"
        "testing compatibility against unknown type";
       (one.base = another.base) 
    || (match (one.base.trait, another.base.trait) with
        | Signed   _ , Univ_int
        | Univ_int   , Signed   _
        | Float    _ , Univ_real
        | Univ_real  , Float    _ 
            -> true
        | _ -> false
        )
