(*
 * ptrtype: do finer typechecks on C pointers
 * Copyright (C) 2012 Etienne Millon
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Etienne Millon <etienne.millon@eads.net>
 * EADS Innovation Works - SE/IT
 * 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
 *)

module T = Tyspeak

type unknown = { id : int }

type var_type =
  | Unknown of unknown
  | Instanciated of simple

and simple =
  | Int
  | Float
  | Fun of simple list * simple list
  | Ptr of simple
  | Array of simple
  | Struct of (int * simple) list ref
  | Var of var_type ref

type variable =
  | VGlobal of string
  | VLocal of string
  | VFun of Newspeak.fid

let string_of_variable = function
  | VGlobal v -> "(g)" ^ v
  | VLocal v -> "(l)" ^ v
  | VFun v -> "(f)" ^ v

let string_of_unknown { id = n } =
    string_of_int n

let rec string_of_simple = function
  | Int -> "Int"
  | Float -> "Float"
  | Ptr s -> "Ptr (" ^ string_of_simple s ^ ")"
  | Array s -> "Array (" ^ string_of_simple s ^ ")"
  | Struct f -> "Struct (" ^ String.concat "," (List.map string_of_field !f) ^ ")"
  | Var {contents = Unknown u} -> "_a"^string_of_unknown u
  | Var {contents = Instanciated s} -> string_of_simple s
  | Fun (args, rets) ->
      let print_lst l =
        "(" ^ String.concat " * " (List.map string_of_simple l) ^ ")"
      in
      print_lst args ^ " -> " ^ print_lst rets

and string_of_field (n, t) =
    Printf.sprintf "%d : %s" n (string_of_simple t)

let rec shorten = function
  | Var ({contents = Instanciated (Var _ as t)} as vt) ->
      let t2 = shorten t in
      vt := Instanciated t;
      t2
  | Var {contents = Instanciated t} -> t
  | t -> t

let rec type_eq x y =
  match (shorten x, shorten y) with
  | Int, Int -> true
  | Float, Float -> true
  | Ptr px, Ptr py -> type_eq px py
  | Var {contents = Unknown nx}, Var {contents = Unknown ny} -> nx = ny
  | Fun (argsa, retsa), Fun (argsb, retsb) ->
        List.for_all2 type_eq argsa argsb
     && List.for_all2 type_eq retsa retsb
  | _ -> false

let rec vars_of_typ = function
  | Int | Float -> []
  | Ptr t | Array t -> vars_of_typ t
  | Var ({contents = Unknown {id = n}}) -> [n]
  | Var ({contents = Instanciated t}) -> vars_of_typ t
  | Fun (args, rets) ->
          List.concat (List.map vars_of_typ (rets@args))
  | Struct f -> List.concat (List.map (fun (_, t) -> vars_of_typ t) !f)

let rec type_eq x y =
  match (shorten x, shorten y) with
  | Int, Int -> true
  | Float, Float -> true
  | Ptr px, Ptr py -> type_eq px py
  | Var {contents = Unknown nx}, Var {contents = Unknown ny} -> nx = ny
  | Fun (argsa, retsa), Fun (argsb, retsb) ->
        List.for_all2 type_eq argsa argsb
     && List.for_all2 type_eq retsa retsb
  | _ -> false

let extract_fun_type = function
  | Fun (a, r) -> (a, r)
  | _ -> invalid_arg "extract_fun_type" 
