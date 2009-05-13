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

let error x = Npkcontext.print_debug ("Ada_types : "^x)

(**
 * The [string] type with primitives to
 * handle it in a case-insensitive way.
 *)
module CaseInsensitiveString =
  struct
    type t = string

    let equal s1 s2 =
      String.compare (String.lowercase s1) (String.lowercase s2) = 0

    let hash s =
      Hashtbl.hash (String.lowercase s)

    let to_string x = x
  end

(** A hash table insensitive to keys' case.  *)
module IHashtbl = Hashtbl.Make(CaseInsensitiveString)

(**
 * A universal type.
 *
 * It is used to make an heterogeneous container by wrapping polymorphism into
 * itself, much like enum+union structures or Object containers in Java (before
 * generics).
 *
 * Each call to [embed ()] yields a couple of functions [inject,project] :
 *   - [inject x] creates a [Univ.t] wrapped value from data [x], which may
 *   have any type.
 *   - [project u] is used to unwrap the value back. It returns an option type ;
 *   more precisely, [None] is used to indicate that a bad unwrapper has been
 *   used to retrive the wrapped value.
 *
 * The current implementation is not thread-safe and may create memory leaks.
 *)
module Univ :
sig
  type t

  val embed: unit -> ('a -> t) * (t -> 'a option)
end = struct
  type t = {
    id : unit ref;
    store : unit -> unit;
  }

  let embed () =
    let id = ref () in
    let r = ref None in
    let put a =
      let o = Some a in
      { id = id; store = (fun () -> r := o); }
      in
      let get t =
        if id == t.id then (t.store
        (); let a = !r in r := None;
        a) else None
        in
        (put, get)
end

(********************
 * Type definitions *
 ********************)

(* effective type + wrapped data *)
type value = t*Univ.t

and t = {
  parent  : t option; (* None indicates a root type *)
  trait   : trait_t;
  uid     : int;      (* Used to make derived types look different *)
  limited : bool;
}

and range =
  | NullRange
  | Range of Newspeak.Nat.t*Newspeak.Nat.t (* min <= max *)

(* type for "traits" (type of types...) *)
and trait_t =
  | Signed of range option            (* Range constraint *)
  | Modular of int                    (* Modulus *)
  | Enumeration of (string*int) list  (* Name-index *)
  | Float of int                      (* Digits *)
  | Array of t*(t list)               (* Component-indexes list *)

type symbol =
  | Variable of value
  | Type     of t

(*
 * A symbol table.
 * Side effects are everywhere (mutable option + [IHashtbl] which comes from the
 * functorial interface of [Hashtbl].
 * The [parent_table] can be changed only once by calling [link_to_parent].
 *)
type table = {
  mutable parent_table : table  option;
          self         : symbol IHashtbl.t;
}

(*********************************
 * Value injector and projectors *
 *********************************)

let (inject_int ,project_int )  = Univ.embed ()
let (inject_bool,project_bool)  = Univ.embed ()
let (inject_nat ,project_nat )  = Univ.embed ()

let from_int  typ ival = typ,(inject_int  ival)
and to_int  v          = project_int  (snd v)

let from_bool typ ival = typ,(inject_bool ival)
and to_bool v          = project_bool (snd v)

let from_nat  typ ival = typ,(inject_nat  ival)
and to_nat  v          = project_nat  (snd v)

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
  let symb_str x = match x with
  | Variable _ -> "var"
  | Type     _ -> "typ"
  in
  let pad width str =
    let x = String.length str in
    let b = (width - x) / 2   in
    let a =  width - x - b    in
    (String.make a ' ')^str^(String.make b ' ')
  in
  print_endline "+----------------------+";
  print_endline "|     Symbol table     |";
  print_endline "+------+---------------+";
  print_endline "| type |      name     |";
  print_endline "+------+---------------+";
  IHashtbl.iter (fun n s ->
                  List.iter print_string
                  ["|"
                  ;pad 6 (symb_str s)
                  ;"|"
                  ;pad 15 n
                  ;"|"
                  ];
                  print_newline ()
               )
      tbl.self;
  print_endline "+------+---------------+";
  print_newline ()

let create_table size = {parent_table=None; self=IHashtbl.create size}

let link_to_parent tbl ~parent =
  match tbl.parent_table with
  | Some _ -> raise (Invalid_argument "Already linked")
  | None -> tbl.parent_table <- Some parent

(*
 * Private global symbol table.
 * It is made available to the rest of the world by
 * builtin_type and builtin_variable. *)
let builtin_table :table = create_table 10

(*
 * Add a symbol to a table, or raise an exception in case of conflict.
 * This could be enhanced in order to support overloading.
 *)
let try_to_add_symbol tbl name s =
  IHashtbl.add tbl.self name s

let add_variable tbl id v   = try_to_add_symbol tbl id (Variable v)

let add_type     tbl id typ = try_to_add_symbol tbl id (Type typ)

let remove_type tbl id = IHashtbl.remove tbl.self id

let rec find_type tbl id =
  let res = begin
    try IHashtbl.find tbl.self id
    with Not_found -> begin
                        match tbl.parent_table with
                        | None      -> raise Not_found
                        | Some ptbl -> Type (find_type ptbl id)
                      end
  end in
    match res with
    | Variable _ -> raise Not_found
    | Type typ   -> typ

let rec find_variable tbl id =
  try
    let res = begin
      try IHashtbl.find tbl.self id
      with Not_found -> begin
                          match tbl.parent_table with
                          | None      -> raise Not_found
                          | Some ptbl -> Variable (find_variable ptbl id)
                        end
    end in
      match res with
      | Type _       -> raise Not_found
      | Variable var -> var
  with Not_found -> error ("Variable "^id^" not found");
                    raise Not_found

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
  limited = false;
}

let universal_integer = { type_stub with trait = Signed None; }

let universal_real = { type_stub with trait = Float 100; }

(*
 * Add (or not) a type to a symbol table.
 *)
let maybe_add ?symboltable ?name typ =
  begin match symboltable,name with
  | None    ,None   -> ()
  | Some _  ,None   -> raise (Invalid_argument  "No name provided")
  | None    ,Some _ -> raise (Invalid_argument "No table provided")
  | Some tbl,Some n -> add_type tbl n typ
  end;
  typ

let new_enumerated ?symboltable ?name values =
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
    | Some tbl -> List.iter (fun (id,ival) ->
                              add_variable tbl
                                           id
                                           (from_int new_type ival)
                            )
                            ivalues
    end;
    maybe_add ?symboltable ?name new_type

let new_derived ?symboltable ?name old =
    maybe_add ?symboltable ?name { old with uid = uid#gen }

(* Parent of parent of parent.  *)
let rec root_parent typ =
    match typ.parent with
        | None -> typ
        | Some p -> root_parent p

let new_unconstr ?symboltable ?name parent =
    maybe_add ?symboltable ?name
    {
      type_stub with
      parent = Some (root_parent parent);
      trait  = Signed None;
      uid = parent.uid;
    }

let new_constr ?symboltable ?name parent r =
    maybe_add ?symboltable ?name
    {
      type_stub with
      parent = Some (root_parent parent);
      trait = Signed (Some r);
      uid = parent.uid;
    }

let new_range ?symboltable ?name r =
  new_constr ?symboltable ?name universal_integer r

let new_modular ?symboltable ?name size =
    maybe_add ?symboltable ?name
    {
      type_stub with
      trait = Modular size
    }

let new_float ?symboltable ?name digits =
    maybe_add ?symboltable ?name
    {
      type_stub with
      trait = Float digits
    }

let new_array ?symboltable ?name component ind =
    maybe_add ?symboltable ?name
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

let builtin_type     = find_type     builtin_table
let builtin_variable = find_variable builtin_table

(* TODO hook it with Ada_config *)
let integer_first = min_int
and integer_last  = max_int

let integer  = new_constr ~symboltable:builtin_table
                          ~name:"integer"
                          universal_integer
                          (integer_first @.. integer_last)

let natural  = new_constr ~symboltable:builtin_table
                          ~name:"natural"
                          integer
                          (0 @.. integer_last)

let positive = new_constr ~symboltable:builtin_table
                          ~name:"positive"
                          integer
                          (1 @.. integer_last)

let boolean = new_enumerated ~symboltable:builtin_table
                             ~name:"boolean"
                             ["true" ; "false"]

let _float = new_float ~symboltable:builtin_table
                       ~name:"float"
                       6

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
| Modular m -> Newspeak.Nat.of_int m
| Array _
| Float _
| Signed None -> raise (Invalid_argument "Type with no size")

let rec attr_get typ attr args =
  match typ.trait, attr with
    | Signed (Some Range(a,_)),"first" -> from_nat typ a
    | Signed (Some Range(_,b)),"last"  -> from_nat typ b
    | Enumeration values, "first"      -> from_int typ (snd (List.hd  values))
    | Enumeration values, "last"       -> from_int typ (snd (List_utils.last
                                                                      values))
    | Array (_,ind) , ("first"|"last"|"length")  ->
        begin
        match args with
        | [] -> attr_get typ attr [from_int integer 1]
        | [nv] -> begin match (to_int nv) with
                        | None -> failwith "Unreachable code"
                        | Some n -> if attr="length" then
                            from_nat universal_integer
                             (length_of (List.nth ind (n-1)))
                          else
                            attr_get (List.nth ind (n-1)) attr []
                  end
        | _ -> raise ( Invalid_argument
                      "Too many arguments")
        end
    | Modular _,      "first" -> from_int typ 0
    | Modular size,    "last" -> from_int typ (size-1)
    | Modular size, "modulus" -> from_int typ size
    | Float digits , "digits" -> from_int universal_integer digits
    | Array _, _
    | Modular _, _
    | Float _ , _
    | Enumeration _ , _
    | Signed (Some Range(_,_)),  _
    | Signed (None)           ,  _
    | Signed (Some NullRange) ,  _
                 -> raise ( Invalid_argument ("No such attribute : '"^attr^"'"))

let (@.) typ attr = attr_get typ attr []

(****************
 * Traits tests *
 ****************)

let is_limited typ =
  typ.limited

let is_integer typ =
  match typ.trait with
  | Array       _
  | Float       _
  | Enumeration _ -> false
  | Modular     _
  | Signed      _ -> true

let is_modular typ =
  match typ.trait with
  | Modular     _ -> true
  | Array       _
  | Enumeration _
  | Float       _
  | Signed      _ -> false

let is_boolean typ =
  root_parent typ == boolean

let is_discrete typ =
  match typ.trait with
  | Signed      _
  | Enumeration _
  | Modular     _ -> true
  | Array       _
  | Float       _ -> false

let whose_component predicate typ = match typ.trait with
  | Array (component,_) -> predicate component
  | _ -> false

let is_numeric typ =
  match typ.trait with
  | Array       _
  | Enumeration _ -> false
  | Modular     _
  | Float       _
  | Signed      _ -> true

let is_one_dim_array typ =
  match typ.trait with
  | Array (_,ind) -> List.length ind = 1
  | Enumeration _
  | Float       _
  | Modular     _
  | Signed      _ -> false

let is_scalar typ =
  match typ.trait with
  | Array       _ -> false
  | Float       _
  | Enumeration _
  | Modular     _
  | Signed      _ -> true

let some_eq o1 o2 =
  match (o1,o2) with
  | Some x, Some y -> x = y
  | _, None
  | None, _ -> false

let typeof(t,_) = t

(* TODO : implement it in Univ ? *)
let (@=?) (_,v1) (_,v2) =
       some_eq (project_int  v1) (project_int  v2)
    || some_eq (project_bool v1) (project_bool v2)
    || some_eq (project_nat  v1) (project_nat  v2)

let (@=) v1 v2 =
     (typeof v1  =  typeof v2)
  && (       v1 @=?        v2)

let _ =
  add_type builtin_table "character" character
