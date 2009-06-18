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

(**
 * Wraps what the "current package" is and which packages are
 * marked using the "with" and "use" constructs.
 *)
class package_manager :
object
    method set_current :Syntax_ada.name -> unit
    method reset_current :unit
    method current    :Syntax_ada.package
    method add_with   :Syntax_ada.name -> unit
    method is_with    :Syntax_ada.package -> bool
    method add_use    :Syntax_ada.name -> unit
    method remove_use :Syntax_ada.name -> unit
    method get_use    :Syntax_ada.package list
    method is_extern :bool
    method as_extern_do :(unit->unit)->unit
    method normalize_name : Syntax_ada.name -> bool -> Syntax_ada.name
    method add_renaming_decl : Syntax_ada.name -> Syntax_ada.name -> unit
end = object (self)
      val mutable current_pkg:Syntax_ada.package                 = []
      val mutable    with_pkg:Syntax_ada.package list            = []
      val             context:(Syntax_ada.package,int) Hashtbl.t = Hashtbl.create 3
      val mutable     extflag:bool                    = false
      val mutable    renaming:(Syntax_ada.name*Syntax_ada.name) list = []

      method set_current (x,y) :unit =
        let p = x@[y] in
          current_pkg <- p

      method reset_current =
          current_pkg <- []

      method current =
          current_pkg

      method add_with (x,y) =
        let p = x@[y] in
          with_pkg <- p::with_pkg

      method is_with pkg =
          List.mem pkg with_pkg

      method add_use (x,y) =
        let p = x@[y] in
        if (self#current <> p) then begin
            if (not (self#is_with p)) then begin
              Npkcontext.report_error "Ada_normalize.add_context"
                ((Ada_utils.ident_list_to_string p)^" is undefined")
            end;
            let old_count = try Hashtbl.find context p with Not_found -> 0 in
            Hashtbl.replace context p (old_count + 1)
        end

      method remove_use (x,y) =
        let p = x@[y] in
        try
          let old_count = Hashtbl.find context p in
          if old_count = 1 then Hashtbl.remove context p
          else Hashtbl.replace context p (old_count - 1)
        with Not_found -> ()

      method get_use =
        Hashtbl.fold (fun pkg _ res -> pkg::res) context []

      method is_extern =
        extflag

      method as_extern_do (f:unit->unit) =
        extflag <- true;
        f ();
        extflag <- false

      method normalize_name (name:Syntax_ada.name) extern =
        try List.assoc name renaming with
        Not_found ->
        if (not extern) then name
        else let (parents,ident) = name in
             let pack = self#current in
               match parents with
                 | []                              -> (pack, ident)
                 | a when a=pack || self#is_with a -> (  a , ident)
                 | _ -> Npkcontext.report_error "pkgmgr.normalize_name"
                 ("unknown package "^(Ada_utils.ident_list_to_string parents))

      (**
       * Algorithm for add_rd (A,B)  (A -> B)
       *
       * If A = B, detect circular dependency.
       * If B is already a pointer to some C, we have something like
       *      A -> B -> C
       * So its equivalent to call (A -> C).
       *
       * Note that in the particular case where C = A (cycle), the recursive
       * call is (A -> A) and the circular dependency will be detected.
       *)
      method add_renaming_decl new_name old_name =
        Npkcontext.print_debug ("add_rd "
                               ^Ada_utils.name_to_string new_name
                               ^" --> "
                               ^Ada_utils.name_to_string old_name
        );
        if (new_name=old_name) then
          Npkcontext.report_error "add_renaming_decl"
                        ("Circular declaration detected for '"
                        ^Ada_utils.name_to_string new_name^"'.")
        else begin
          try
            let pointee = List.assoc new_name renaming in
            self#add_renaming_decl new_name pointee
          with Not_found ->
            renaming <- (new_name,old_name)::renaming
        end
  end

let error x =
  Npkcontext.print_debug ("ERROR : Ada_types"^x)

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

(**
 * Abstract specification for function/procedure parameters.
 *)
type f_param = { fp_name : string
               ; fp_in   : bool
               ; fp_out  : bool
               ; fp_type : Ada_types.t
               }

let to_fparam (a,b,c,d) =
  { fp_name = a
  ; fp_in   = b
  ; fp_out  = c
  ; fp_type = d
  }

let from_fparam f =
  ( f.fp_name
  , f.fp_in
  , f.fp_out
  , f.fp_type
  )

(**
 * Symbols.
 *)
type symbol =
  | Variable   of Ada_types.t
  | Type       of Ada_types.t
  | Subprogram of ((f_param list)*Ada_types.t option)
  | Unit       of table

(**
 * A symbol table.
 * t_var and t_type hold respectively data related
 * to variables and returning their type and to types.
 *
 * TODO : However it is not type-safe, as different
 * information is typed the same way.
 *)

and table = { mutable renaming : (string*string) list
            ; t_tbl    : symbol IHashtbl.t
            ; pkgmgr   : package_manager
            }

type symbol_type = SymV | SymT | SymS | SymU

let type_of_sym = function
  | Variable   _ -> SymV
  | Type       _ -> SymT
  | Subprogram _ -> SymS
  | Unit       _ -> SymU

let print_symbol = function
  | Variable     t   -> "V",Ada_types.print t
  | Type         t   -> "T",Ada_types.print t
  | Subprogram (p,r) -> "S",(
                             let pdesc = " with "
                                       ^ string_of_int (List.length p)
                                       ^ " parameter(s)"
                             in match r with None   -> "procedure"^pdesc
                                           | Some t -> "function"
                                                       ^pdesc
                                                       ^" and return type "
                                                       ^Ada_types.print t
                            )
                            
  | Unit         _   -> "U","<unit>"

let print_symbol_join s =
  let (s1,s2) = print_symbol s in
  s1^" "^s2

let syms_of_type t l =
  List.exists (fun x -> (type_of_sym x = t)) l

exception ParameterlessFunction of Ada_types.t

(* ('a -> 'b option) -> ?filter:('b->bool) -> 'a list -> 'b option *)
let rec extract_unique p ?(filter:'b->bool=fun _ -> true) l =
  let p' x =
    match p x with
      | None   -> None
      | Some y -> if filter y then Some y else None
  in
  match l with
  |  []  -> raise Not_found
  | h::t -> begin
              match p' h with
                | None -> extract_unique p' t
                | Some r -> if List.exists (fun x -> p' x <> None) t
                            then None
                            else Some r
            end

let rec cast_v ?(filter=fun _ -> true) lst =
  if (List.length lst > 1) then
  Npkcontext.print_debug ("possible interpretations (variable) = {\n"
                         ^String.concat ",\n" (List.map print_symbol_join lst)
                         ^"}"
                         );
  match extract_unique ~filter (function Variable x -> Some x
                               | Subprogram ([],Some rt) -> raise (ParameterlessFunction rt)
                               |_ -> None
    ) lst with
    | None -> error "cast_variable : Ambiguous variable name";Ada_types.unknown
    | Some x -> x

let rec cast_t lst =
  Npkcontext.print_debug ("possible interpretations (type) = {"
                         ^String.concat ", " (List.map print_symbol_join lst)
                         ^"}");
  match extract_unique (function Type x -> Some x |_ -> None) lst with
  | None -> error "cast_type : Ambiguous type name";Ada_types.unknown
  | Some x -> x

let rec cast_s lst =
  Npkcontext.print_debug ("possible interpretations (subprogram) = {"
                         ^String.concat ", " (List.map print_symbol_join lst)
                         ^"}"
                         );
 match extract_unique (function Subprogram x -> Some x | _ -> None) lst with
   | None -> error "cast_subprogram : Ambiguous subprogram name";[],None
   | Some x -> x

let rec cast_u lst =
  Npkcontext.print_debug ("possible interpretations (unit) = {"
                         ^String.concat ", " (List.map print_symbol_join lst)
                         ^"}"
                         );
  match lst with
  | Unit       x::tl when not (syms_of_type SymU tl) -> x
  |       _     ::tl -> cast_u tl
  |             []   -> raise Not_found

let print_table tbl =
  let pad width str =
    let x = String.length str in
    let b = (width - x) / 2   in
    let a =  width - x - b    in
    (String.make a ' ')^str^(String.make b ' ')
  in
  let line_printer (p,i) sym =
    let (t,sym_d) = print_symbol sym in
    List.iter print_string
    ["|"
    ;pad 6 t
    ;"|"
    ;pad 15 (String.concat "." (p@[i]))
    ;"| "
    ;sym_d
    ];
    print_newline ()
  in
  print_endline "+--------------------------- . . .";
  print_endline "|           Symbol table          ";
  print_endline "+------+---------------+---- . . .";
  print_endline "| Type |     Name      |  Contents";
  print_endline "+------+---------------+---- . . .";
  IHashtbl.iter line_printer tbl.t_tbl;
  print_endline "+------+---------------+---- . . .";
  print_newline ()

(**
 * Private global symbol table.
 * It is made available to the rest of the world by
 * builtin_type and builtin_variable.
 *)
let builtin_table :table = { renaming = []
                           ; t_tbl    = IHashtbl.create 0
                           ; pkgmgr   = new package_manager
                           }

let create_table _ =  { renaming = []
                      ; t_tbl    = IHashtbl.copy builtin_table.t_tbl
                      ; pkgmgr   = new package_manager
                      }

let add_variable tbl n t =
  Npkcontext.print_debug ("Adding variable "^String.concat "." (fst n@[snd n]));
  IHashtbl.add tbl.t_tbl n (Variable t)

let add_type tbl n typ =
  Npkcontext.print_debug ("Adding type  "   ^String.concat "." (fst n@[snd n]));
  IHashtbl.add tbl.t_tbl n (Type typ)

let add_subprogram tbl name params rettype =
  IHashtbl.add tbl.t_tbl name (Subprogram (List.map to_fparam params,rettype))

(**
 * Find matching symbols in a table.
 *
 * First, look for renaming declarations.
 * Then, it depends on the package provided :
 *   If some package is provided, find directly this one.
 *   If no package is provided (empty list) :
 *     Try it first as a local variable
 *     Then, try it with every package provided in the context.
 *     (context = current package + use list).
 *
 * The return value is a list of [symbol]s for the first class that returns a
 * non-empty result. This approach allows to correctly handle hiding.
 *
 * This result is to be extracted by find_xxx functions
 * that will call cast_x on it.
 *)
let rec find_symbols t (package,id) =
  let ren = try find_symbols t (package, List.assoc id t.renaming)
            with Not_found -> []
  in
  if ren <> [] then ren
  else
  begin
    if package <> [] then IHashtbl.find_all t.t_tbl (package,id)
    else 
      let as_local = IHashtbl.find_all t.t_tbl ([],id) in
      if as_local <> [] then as_local else
        begin
          try
            let p = List.find (fun pkg -> IHashtbl.mem t.t_tbl (pkg,id))
                              (t.pkgmgr#current::t.pkgmgr#get_use)
            in
            IHashtbl.find_all t.t_tbl (p,id)
          with Not_found -> (* thrown by List.find if no result is found *)
            []
        end
  end

let find_type tbl n =
  try cast_t (find_symbols tbl n)
  with Not_found -> begin
                      error ("Cannot find type "
                            ^String.concat "." (fst n@[snd n])
                            );
                      Ada_types.unknown
                    end

let type_ovl_intersection tbl n1 n2 =
  let inter l1 l2 =
    List.filter (fun x -> List.mem x l1) l2
  in
  let s1 = find_symbols tbl n1 in
  let s2 = find_symbols tbl n2 in
  let inte = inter s1 s2 in
  if inte = [] then Npkcontext.print_debug "Binop : intersection empty" else ();
  cast_v inte

let find_subprogram tbl n =
  try (fun (x,y) -> List.map from_fparam x,y)
      (cast_s (find_symbols tbl n))
  with Not_found -> begin
                      error ("Cannot find subprogram "
                            ^String.concat "." (fst n@[snd n])
                            );
                      raise Not_found
                    end

let find_variable tbl ?expected_type n =
  let ovl_predicate = match expected_type with
    | Some t when t <> Ada_types.unknown
        -> fun x ->     Ada_types.is_compatible x t
    | _ -> fun _ -> true
  in
  begin
  Npkcontext.print_debug ("Find_variable "
                         ^snd n
                         ^" : expected type is "
                         ^match expected_type with None -> "None"
                                  | Some t -> Ada_types.print t)
  end;
  try cast_v ~filter:ovl_predicate (find_symbols tbl n)
  with Not_found ->
    begin
       error ("Cannot find variable "
             ^String.concat "." (fst n@[snd n])
             );
       Ada_types.unknown
    end

let add_renaming_declaration t newname oldname =
  t.renaming <- (newname,oldname)::t.renaming

let builtin_type x = find_type builtin_table ([],x)

let set_current       t = t.pkgmgr#set_current
let reset_current     t = t.pkgmgr#reset_current
let current           t = t.pkgmgr#current
let add_with          t = t.pkgmgr#add_with          
let is_with           t = t.pkgmgr#is_with           
let add_use           t = t.pkgmgr#add_use           
let remove_use        t = t.pkgmgr#remove_use        
let get_use           t = t.pkgmgr#get_use           
let is_extern         t = t.pkgmgr#is_extern         
let as_extern_do      t = t.pkgmgr#as_extern_do      
let normalize_name    t = t.pkgmgr#normalize_name    
let add_renaming_decl t = t.pkgmgr#add_renaming_decl 

let _ =
  List.iter (fun (n,t) -> add_type builtin_table ([],n) t)
  ["integer"  , Ada_types.integer
  ;"float"    , Ada_types.std_float
  ;"boolean"  , Ada_types.boolean
  ;"natural"  , Ada_types.natural
  ;"positive" , Ada_types.positive
  ;"character", Ada_types.character
  ]

module Stack = struct
  type t = table Stack.t

  let create _ =
    let s = Stack.create () in
    Stack.push builtin_table s;
    let library = create_table () in
    Stack.push library s;
    s

  let top x = Stack.top x

  let enter_context s name_opt =
    let new_context = create_table () in
    begin match name_opt with
      | None      -> ()
      | Some name -> IHashtbl.add ((Stack.top s).t_tbl) ([],name) (Unit new_context)
    end;
    Stack.push new_context s

  let new_unit s name =
    while (Stack.length s > 2) do
      ignore (Stack.pop s);
    done;
    (* Here Stack.length s = 2 so we are at library level *)
    enter_context s (Some name)

  let exit_context x = ignore (Stack.pop x)
end
