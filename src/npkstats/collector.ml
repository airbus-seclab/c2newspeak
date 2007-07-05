(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
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

  Charles Hymans
  EADS Innovation Works - SE/CS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: charles.hymans@penjili.org

  Olivier Levillain
  email: olivier.levillain@penjili.org
*)


open Newspeak

let size_of = ref (fun _ -> invalid_arg "Collector.sizeof: Not initialized")

let init ptr_sz = 
  let (_, f) = Newspeak.create_size_of ptr_sz in
    size_of := f

class collector =
object 
  inherit Newspeak.nop_visitor
    
  method process_unop x =
    match x with
	Belongs _ -> Npkstats.count Npkstats.array
      | _ -> ()

  method process_binop x =
    match x with
	PlusPI -> Npkstats.count Npkstats.pointer_arith
      | _ -> ()

  method process_lval x =
    let _ =
      match x with
	  Deref _ -> Npkstats.count Npkstats.pointer_deref
	| _ -> ()
    in
      true

  method process_fn x =
    let _ =
      match x with
	  FunId f -> Npkstats.count_call f
	| FunDeref (e, _) -> Npkstats.count Npkstats.fpointer
    in
      true

  method process_stmt (x, _) =
    Npkstats.count Npkstats.instrs;
    let _ = 
      match x with
	  InfLoop body -> Npkstats.count Npkstats.loop
	| _ -> ()
    in
      true

  method process_fun _ (_, x) =
    let _ = 
      match x with
	  Some _ -> Npkstats.count Npkstats.funct
	| _ -> ()
    in
      true

  method process_gdecl (_, t, _) =
    Npkstats.count Npkstats.globals;
    Npkstats.incr_counter Npkstats.bytes (!size_of t);
    true

end

let count ptr_sz prog =
  init ptr_sz;
  Newspeak.visit (new collector) prog
