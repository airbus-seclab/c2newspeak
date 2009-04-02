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
  email : etienne.millon AT gmail.com

*)
open Syntax_ada

(* FIXME *)
let d_package_body    b = b

(* FIXME *)
let d_subprogram_body b = b

(* FIXME *)
let d_subprogram_spec s = s

(* FIXME *)
let d_basic_decl x = x

let d_package_spec    (n,l) =
  let d_l = List.map
              (fun (bd,loc) ->
                d_basic_decl bd,loc
              )
              l
  in
  n,d_l

let d_library =
  function
  | Spec SubProgramSpec ss -> Spec (SubProgramSpec (d_subprogram_spec ss))
  | Spec    PackageSpec ps -> Spec (PackageSpec    (d_package_spec    ps))
  | Body SubProgramBody sb -> Body (SubProgramBody (d_subprogram_body sb))
  | Body    PackageBody pb -> Body (PackageBody    (d_package_body    pb))

let decorate (ctx,library,loc) =
  ctx,d_library library,loc
