(* FIXME insert license *)

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

type value = t*Univ.t

(* Type and type table definition *)
and t = {
  parent  : t option;
  trait   : trait_t;
  uid     : int;
  limited : bool
}

and range =
  | NullRange
  | Range of int*int (* min <= max *)

(** type for "traits" (type of types...) *)
and trait_t =
  | Signed of range option
  | Modular of int
  | Enumeration of (string*int) list
  | Float of int (* digits *)
  | Array of t*(t list)

(*********************************
 * Value injector and projectors *
 *********************************)

(* TODO merge globals together *)
let (inject_int ,project_int)  = Univ.embed ()
let (inject_char,project_char) = Univ.embed ()
let (inject_bool,project_bool) = Univ.embed ()

let from_int typ ival = typ,(inject_int ival)
and to_int v          = project_int (snd v)

(***********
 * Symbols *
 ***********)

type symbol =
  | Variable of value
  | Type     of t

type table = (string, symbol) Hashtbl.t

let print_table tbl =
  let symb_str x = match x with
  | Variable _ -> "var"
  | Type     _ -> "typ"
  in
  let pad width str =
    let x = String.length str in
    let b = (width - x) / 2 in
    let a = width-x-b in
    (String.make a ' ')^str^(String.make b ' ')
  in
  print_endline "+----------------------+";
  print_endline "|     Symbol table     |";
  print_endline "+------+---------------+";
  print_endline "| type |      name     |";
  print_endline "+------+---------------+";
  Hashtbl.iter (fun n s ->
                  print_char '|';
                  print_string (pad 6 (symb_str s));
                  print_char '|';
                  print_string (pad 15 n);
                  print_char '|';
                  print_newline ();
  ) tbl;
  print_endline "+------+---------------+";
  print_newline()

let create_table size =
  Hashtbl.create size

let try_to_add_symbol tbl name s =
  if Hashtbl.mem tbl name then
    raise (Invalid_argument ("Name '"^name^"' already exists"))
  else
    Hashtbl.add tbl name s


let add_variable tbl id v =
  try_to_add_symbol tbl id (Variable v)

let add_type     tbl id typ =
  try_to_add_symbol tbl id (Type typ)

let find_type tbl id = match (Hashtbl.find tbl id) with
| Variable _ -> raise Not_found
| Type typ   -> typ

let find_variable tbl id = match (Hashtbl.find tbl id) with
| Type _     -> raise Not_found
| Variable v -> v

let uid = object
  val mutable count = 0;
  method gen =
    count <- count + 1;
    count
end

let type_stub = {
  parent  = None;
  uid     = 0;
  trait   = Signed None;
  limited = false;
}

let null_range = NullRange

let (@..) min max =
  if min>max then NullRange
  else Range (min,max)

let sizeof = function
  | NullRange       -> 0
  | Range (min,max) -> (max - min + 1)

let maybe_add ?symboltable ?name typ =
  begin match symboltable,name with
  | None    ,None   -> ()
  | Some _  ,None   -> raise (Invalid_argument  "No name provided")
  | None    ,Some _ -> raise (Invalid_argument "No table provided")
  | Some tbl,Some n -> add_type tbl n typ
  end;
  typ

(**
 * An enumerated type is like a discrete range type flagged as incompatible
 * with other ones.
 * Litterals may be added as variables in a symbol table.
 *)
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

(* Builtin types *)

let builtin_table :table = create_table 10

let builtin_type     = find_type     builtin_table
let builtin_variable = find_variable builtin_table

let __universal_integer = { type_stub with trait = Signed None; }

let integer_first = min_int
and integer_last  = max_int

let integer  = new_constr ~symboltable:builtin_table
                          ~name:"integer"
                          __universal_integer
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

(* don't add __char_x litterals *)
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
| Enumeration vals -> List.length vals
| Modular m -> m
| Array _
| Float _
| Signed None -> raise (Invalid_argument "Type with no size")

let rec attr_get typ attr args =
  match typ.trait, attr with
    | Signed (Some Range(a,_)),"first"  -> from_int typ a
    | Signed (Some Range(_,b)),"last"   -> from_int typ b
    | Enumeration values, "first"       -> from_int typ (snd (List.hd values))
    | Enumeration values, "last"        -> from_int typ (snd (List.nth values
                                                        ((List.length values)-1)
                                                          ))
    | Array (_,ind) , ("first"|"last"|"length")  ->
        begin
        match args with
        | [] -> attr_get typ attr [from_int integer 1]
        | [nv] -> begin match (to_int nv) with
                        | None -> failwith "Unreachable code"
                        | Some n -> if attr="length" then
                            from_int __universal_integer
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
    | Float digits , "digits" -> from_int __universal_integer digits
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

let is_modular typ =
  match typ.trait with
  | Modular     _ -> true
  | Array       _
  | Enumeration _
  | Float       _
  | Signed      _ -> false

(* OneDimenSionnalArrayWhoseComponentIsOfABooleanType *)
let is_odawcioa_boolean_t typ =
  match typ.trait with
  | Array       _ (* FIXME *)
  | Modular     _
  | Float       _
  | Enumeration _
  | Signed      _ -> false

let is_boolean typ =
  root_parent typ == boolean

let is_odawcioa_discrete_t typ =
  match typ.trait with
  | Array       _ (* FIXME *)
  | Modular     _
  | Float       _
  | Enumeration _
  | Signed      _ -> false

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

let is_integer typ =
  match typ.trait with
  | Array       _
  | Float       _
  | Enumeration _ -> false
  | Modular     _
  | Signed      _ -> true

let is_scalar typ =
  match typ.trait with
  | Array       _ -> false
  | Float       _
  | Enumeration _
  | Modular     _
  | Signed      _ -> true

(* for unary operators : "+"; "-"; "abs"; "not" *)
let operator_exists typ opname =
match opname with
| "and" | "or" | "xor" ->    is_boolean typ
                          || is_modular typ
                          || is_odawcioa_boolean_t typ

| "="  | "/="          -> not (is_limited typ)

| "<"  | "<="| ">"| ">=" ->    is_scalar typ
                            || is_odawcioa_discrete_t typ

| "+"  | "-"             -> is_numeric typ

| "&" -> not (is_limited typ)
          && is_one_dim_array typ

| "*"  | "/"  | "mod"| "rem" -> is_integer typ
                            (* TODO + specific *)
(* | "**" ->  TODO*)
|_ -> failwith "Invalid operator name"

let some_eq o1 o2 =
  match (o1,o2) with
  | Some x, Some y -> x = y
  | _, None
  | None, _ -> false

let typeof(t,_) = t

(* TODO : implement it in Univ *)
let (@=?) (_,v1) (_,v2) =
       some_eq (project_int  v1) (project_int  v2)
    || some_eq (project_bool v1) (project_bool v2)

let (@=) v1 v2 =
     (typeof v1  =  typeof v2)
  && (       v1 @=?        v2)


let _ =
  add_type builtin_table "character" character
