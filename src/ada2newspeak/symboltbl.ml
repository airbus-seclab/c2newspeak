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

(**
 * Abstract specification for function/procedure parameters.
 *)
type f_param = { fp_name : string
               ; fp_in   : bool
               ; fp_out  : bool
               ; fp_type : Ada_types.t
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
  t_var  : Ada_types.t IHashtbl.t;
  t_type : Ada_types.t IHashtbl.t;
  t_func : ((f_param list)*Ada_types.t option) IHashtbl.t
}

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
    ;Ada_types.print s_t
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

let create_table _ =  { t_var  = IHashtbl.copy builtin_table.t_var
                      ; t_type = IHashtbl.copy builtin_table.t_type
                      ; t_func = IHashtbl.copy builtin_table.t_func
                      }

let add_variable tbl n t =
  IHashtbl.add tbl.t_var n t

let add_type tbl n typ =
  IHashtbl.add tbl.t_type n typ

let add_subprogram tbl name params rettype =
  IHashtbl.add tbl.t_func name (List.map (fun (a,b,c,d) ->
    { fp_name = a
    ; fp_in   = b
    ; fp_out  = c
    ; fp_type = d
    }) params,rettype)

let remove_type tbl id = IHashtbl.remove tbl.t_type id

let find_information hashtbl ?context (package,id) =
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

let find_type tbl ?context n =
  try find_information tbl.t_type ?context n
  with Not_found -> begin
                      error ("Cannot find type "^snd n);
                      raise Not_found
                    end

let find_variable tbl ?context n =
  try find_information tbl.t_var ?context n
  with Not_found -> begin
                      error ("Cannot find variable "^snd n);
                      Ada_types.unknown
                    end

let builtin_type x = find_type builtin_table ([],x)

let _ =
  List.iter (fun (n,t) -> add_type builtin_table ([],n) t)
  ["integer"  , Ada_types.integer
  ;"float"    , Ada_types.std_float  
  ;"boolean"  , Ada_types.boolean 
  ;"natural"  , Ada_types.natural 
  ;"positive" , Ada_types.positive 
  ;"character", Ada_types.character
  ]

