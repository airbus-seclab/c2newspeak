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

let error x =
  if 0=1 then
    Npkcontext.report_warning "Ada_types" x

(**
 * The [string] type with primitives to
 * handle it in a case-insensitive way.
 *)
module CaseInsensitiveString =
  struct
    type t = string list*string

    let equal_s s1 s2 =
      String.compare (String.lowercase s1) (String.lowercase s2) = 0

    let equal (p1,i1) (p2,i2) =
      equal_s i1 i2 &&
      try List.for_all2 equal_s p1 p2
      with Invalid_argument _ -> false

    let hash_s s =
      Hashtbl.hash (String.lowercase s)

    let hash (p,i) =
      List.fold_left (fun x y -> x+hash_s y) (hash_s i) p

    let to_string x = x
  end

(** A (string list*string) hash table insensitive to keys' case.  *)
module IHashtbl = Hashtbl.Make(CaseInsensitiveString)

(********************
 * Type definitions *
 ********************)

type data_t =
  | Int of int
  | Nat of Newspeak.Nat.t

(* effective type + wrapped data *)
type value = t*data_t

and t = {
  parent  : t option; (* None indicates a root type *)
  trait   : trait_t;
  uid     : int;      (* Used to make derived types look different *)
}

and range =
  | NullRange
  | Range of Newspeak.Nat.t*Newspeak.Nat.t (* min <= max *)

(* type for "traits" (type of types...) *)
and trait_t =
  | Signed of range option            (* Range constraint *)
  | Enumeration of (string*int) list  (* Name-index *)
  | Float of int                      (* Digits *)
  | Array of t*(t list)               (* Component-indexes list *)

(**
 * Abstract specification for function/procedure parameters.
 *)
type f_param = { fp_name : string
               ; fp_in   : bool
               ; fp_out  : bool
               ; fp_type : t
               }

(**
 * A symbol table.
 * t_var and t_type hold respectively data related
 * to variables and returning their type and to types.
 *
 * TODO : However it is not type-safe, as different
 * information is typed the same way.
 *)

type table = {
  t_var  : t IHashtbl.t;
  t_type : t IHashtbl.t;
  t_func : ((f_param list)*t option) IHashtbl.t
}

(***************************
 * Value (de-)constructors *
 ***************************)

let from_int t x = (t, Int x)
let from_nat t x = (t, Nat x)

let to_int = function
  | (_, Int v) -> Some v
  | _          -> None

let to_nat = function
  | (_, Nat v) -> Some v
  | _          -> None

(******************
 * Pretty-printer *
 ******************)

let print t =
  let p_hash t  = Printf.sprintf "%08x" (Hashtbl.hash t) in
  let p_parent = function
    | None -> "<root>"
    | Some t -> "parent:<"^p_hash t^">"
  in
  let p_range = function
    | None      -> "<unlimited>"
    | Some NullRange     -> "{}"
    | Some (Range (a,b)) ->  "["
                            ^   Newspeak.Nat.to_string a
                            ^";"
                            ^   Newspeak.Nat.to_string b
                            ^"]"
  in
  let p_trait = function
    | Signed  r -> "Signed " ^p_range r
    | Float   d -> "Float "  ^string_of_int d
    | Enumeration v -> "Enum (length ="^string_of_int (List.length v)^")"
    | Array (c,i) -> "Array {{ component.hash = "
                  ^p_hash c^"; indexes.hash = "
                  ^String.concat "," (List.map p_hash i)
                  ^"}}"
  in
   "{"
  ^"H="^p_hash t
  ^",parent = "^p_parent t.parent
  ^"U="^string_of_int t.uid
  ^",trait = "^p_trait t.trait
  ^"}"

(**********
 * Ranges *
 **********)

let null_range = NullRange

let (@...) min max =
  if min>max then NullRange
  else Range (min,max)

let (@..) min max =
  (Newspeak.Nat.of_int min) @... (Newspeak.Nat.of_int max)

let sizeof = function
  | NullRange       -> Newspeak.Nat.zero
  | Range (min,max) -> (max %- min %+ Newspeak.Nat.one)

(*****************
 * Symbol tables *
 *****************)

let print_table tbl =
  let pad width str =
    let x = String.length str in
    let b = (width - x) / 2   in
    let a =  width - x - b    in
    (String.make a ' ')^str^(String.make b ' ')
  in
  let line_printer t (p,i) s_t =
    List.iter print_string
    ["|"
    ;pad 6 t
    ;"|"
    ;pad 15 (String.concat "." (p@[i]))
    ;"| "
    ;print s_t
    ];
    print_newline ()
  in
  print_endline "+--------------------------- . . .";
  print_endline "|           Symbol table          ";
  print_endline "+------+---------------+---- . . .";
  print_endline "| type |      name     |  Contents";
  print_endline "+------+---------------+---- . . .";
  IHashtbl.iter (line_printer "Var") tbl.t_var;
  IHashtbl.iter (line_printer "Typ") tbl.t_type;
  print_endline "+------+---------------+---- . . .";
  print_newline ()

(**
 * Private global symbol table.
 * It is made available to the rest of the world by
 * builtin_type and builtin_variable.
 *)
let builtin_table :table = { t_var  = IHashtbl.create 0
                           ; t_type = IHashtbl.create 0
                           ; t_func = IHashtbl.create 0
                           }

let create_table _size =  { t_var  = IHashtbl.copy builtin_table.t_var
                          ; t_type = IHashtbl.copy builtin_table.t_type
                          ; t_func = IHashtbl.copy builtin_table.t_func
                          }

let add_variable tbl package id t =
  let descr p =
    if p = [] then "as a local variable"
    else "in package "^String.concat "." p
  in
  Npkcontext.print_debug ("SYMBTBL --> adding variable "
    ^id
    ^" "^descr package);
  IHashtbl.add tbl.t_var (package,id) t

let add_type tbl package id typ =
  Npkcontext.print_debug ("SYMBTBL --> adding type "^id);
  IHashtbl.add tbl.t_type (package,id) typ

let add_subprogram tbl name params rettype =
  IHashtbl.add tbl.t_func name (List.map (fun (a,b,c,d) ->
    { fp_name = a
    ; fp_in   = b
    ; fp_out  = c
    ; fp_type = d
    }) params,rettype)

let remove_type tbl id = IHashtbl.remove tbl.t_type id

let find_information hashtbl ?context package id =
  if package != [] then IHashtbl.find hashtbl (package,id)
  else (* no package : try it as a local variable,
        *              or within current package
        *              (if provided)
        *)
    try IHashtbl.find hashtbl ([],id)
    with Not_found ->
      begin
        match context with
        | None   -> raise Not_found
        | Some package_list ->
          let p = List.find (fun pkg -> IHashtbl.mem hashtbl (pkg,id))
                            package_list
            (* List.find throws Not_found if     *
             * no package matches the predicate. *
             * This is the expected behaviour.   *)
          in
          IHashtbl.find hashtbl (p,id)
      end

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

(*
 * The minimal data for a type.
 * For declaring a new type (inside constructors...), one should write
 * [{type_stub with ..}]
 *)
let type_stub = {
  parent  = None;
  uid     = 0;
  trait   = Signed None;
}

let unknown = {type_stub with trait = Signed (Some NullRange)}

let universal_integer = { type_stub with trait = Signed None; }

let universal_real = { type_stub with trait = Float 100; }


let find_type tbl ?context package id =
  try find_information tbl.t_type ?context package id
  with Not_found -> begin
                      error ("Cannot find type "^id);
                      raise Not_found
                    end

let find_variable tbl ?context package id =
  try find_information tbl.t_var ?context package id
  with Not_found -> begin
                      error ("Cannot find variable "^id);
                      universal_integer
                    end

let new_enumerated ?symboltable values =
    let rec with_indices vals offset = match vals with
    |   []  -> []
    | n::tl -> (n,offset)::with_indices tl (offset+1)
    in
    let ivalues = with_indices values 0 in
    let rec new_type = {
      type_stub with
      trait = Enumeration ivalues;
    } in
    begin match symboltable with
    | None -> ()
    | Some tbl -> List.iter (fun (id,_) ->
                              add_variable tbl
                                           []
                                           id
                                           new_type
                            )
                            ivalues
    end;
    new_type

let new_derived old =
    { old with uid = uid#gen }

(* Parent of parent of parent.  *)
let rec root_parent typ =
    match typ.parent with
        | None -> typ
        | Some p -> root_parent p

let new_unconstr parent =
    {
      parent = Some (root_parent parent);
      trait  = Signed None;
      uid = parent.uid;
    }

let new_constr parent r =
    {
      parent = Some (root_parent parent);
      trait = Signed (Some r);
      uid = parent.uid;
    }

let new_range r =
  new_constr universal_integer r

let new_float digits =
    {
      type_stub with
      trait = Float digits
    }

let new_array component ind =
    {
      type_stub with
      trait = Array (component,ind)
    }

let is_compatible one another =
    match (one.parent,another.parent) with
      | Some p1, Some p2 -> (p1 = p2 && one.uid = another.uid)
      | _                -> false

(*****************
 * Builtin types *
 *****************)

(* TODO hook it with Ada_config *)
let integer_first = min_int
and integer_last  = max_int

let integer  = new_constr universal_integer
                          (integer_first @.. integer_last)

let natural  = new_constr integer
                          (0 @.. integer_last)

let positive = new_constr integer
                          (1 @.. integer_last)

let boolean = new_enumerated ~symboltable:builtin_table ["true" ; "false"]

let std_float = new_float 6

let builtin_type x = find_type builtin_table [] x

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

(* Number of values in a type *)
let length_of typ = match typ.trait with
| Signed (Some r) -> sizeof r
| Enumeration vals -> Newspeak.Nat.of_int (List.length vals)
| Array _
| Float _
| Signed None -> raise (Invalid_argument "Type with no size")

let rec attr_get typ attr =
  match typ.trait, attr with
    | Signed (Some Range(a,_)),"first" -> from_nat typ a
    | Signed (Some Range(_,b)),"last"  -> from_nat typ b
    | Enumeration values, "first"      -> from_int typ (snd (List.hd  values))
    | Enumeration values, "last"       -> from_int typ (snd (List_utils.last
                                                                      values))
    | Array (_,ind) , ("first"|"last"|"length")  ->
        begin
          if attr="length" then
            from_nat universal_integer
              (length_of (List.hd ind ))
          else
            attr_get (List.hd ind) attr
        end
    | Float digits , "digits" -> from_int universal_integer digits
    | Array _, _
    | Float _ , _
    | Enumeration _ , _
    | Signed (Some Range(_,_)),  _
    | Signed (None)           ,  _
    | Signed (Some NullRange) ,  _
                 -> raise ( Invalid_argument ("No such attribute : '"^attr^"'"))

(****************
 * Traits tests *
 ****************)

let is_integer typ =
  match typ.trait with
  | Array       _
  | Float       _
  | Enumeration _ -> false
  | Signed      _ -> true

let is_boolean typ =
  root_parent typ == boolean

let is_discrete typ =
  match typ.trait with
  | Signed      _
  | Enumeration _ -> true
  | Array       _
  | Float       _ -> false

let is_numeric typ =
  match typ.trait with
  | Array       _
  | Enumeration _ -> false
  | Float       _
  | Signed      _ -> true

let is_scalar typ =
  match typ.trait with
  | Array       _ -> false
  | Float       _
  | Enumeration _
  | Signed      _ -> true

let _ =
  List.iter (fun (n,t) -> add_type builtin_table [] n t)
  ["integer"  , integer
  ;"float"    , std_float  
  ;"boolean"  , boolean 
  ;"natural"  , natural 
  ;"positive" , positive 
  ;"character", character
  ]

