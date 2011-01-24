(*
  C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
  well-suited for static analysis.
  Copyright (C) 2007-2011  Charles Hymans, Olivier Levillain, Sarah Zennou
  
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

  Sarah Zennou
  EADS Innovation Works - SE/IS
  12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
  email: sarah(dot)zennou(at)eads(dot)net
*)

open Lowspeak
module N = Newspeak

let debug = ref false

let verbose = ref false

let obfuscate = ref false

let fun_to_count = ref []

let graphs = ref false

let funstats = ref false

let output = ref "a"

let xout = ref ""

let add_counted_call f = fun_to_count := f::!fun_to_count

let more_verb = ref false

let speclist = 
  [("--count-call fid", Arg.String add_counted_call, 
    "count the number of calls to function fid");

   ("--verbose", Arg.Set verbose, 
    "prints out the detailed statistics for each function");
  
   ("--obfuscate", Arg.Set obfuscate, "obfuscates output");
   
   ("--csv", Arg.Set graphs, "generates data into csv format");

   ("-o", Arg.Set_string output, "changes name of output, default is 'a'");

   ("--debug", Arg.Set debug, "debugging mode");
   
   ("--more-verb", Arg.Set more_verb, "more statistics");

   ("--funstats", Arg.Set funstats, "more statistics on functions");
   
   ("--xml", Arg.Set_string xout, "generates data into xml format")
  ]

type counters = 
    { mutable instrs	   : int; 
      mutable loop	   : int; 
      mutable array	   : int;
      mutable pointer_deref: int; 
      mutable pointer_arith: int;
      mutable fpointer	   : int
    }

let init_counters () = 
  { instrs = 0; loop = 0; array = 0; pointer_deref = 0; pointer_arith = 0;
    fpointer = 0 }

let incr_counters dest src =
  dest.instrs 	     <- dest.instrs + src.instrs;
  dest.loop 	     <- dest.loop + src.loop;
  dest.array 	     <- dest.array + src.array;
  dest.pointer_deref <- dest.pointer_deref + src.pointer_deref;
  dest.pointer_arith <- dest.pointer_arith + src.pointer_arith;
  dest.fpointer      <- dest.fpointer + src.fpointer

let string_of_counters counters b =
  let s1 = "Number of instructions" in
  let n1 = string_of_int counters.instrs in
  let s2 = "Number of loops" in
  let n2 = string_of_int counters.loop in
  let s3 = "Number of array operations" in
  let n3 = string_of_int counters.array in
  let s4 = "Number of pointer deref" in
  let n4 = string_of_int counters.pointer_deref in
  let s5 = "Number of pointer arithmetic (+)" in
  let n5 = string_of_int counters.pointer_arith in
  let s6 = "Number of function pointer call" in
  let n6 = string_of_int counters.fpointer in
  let out = s1^": "^n1^"\n"^s2^": "^n2^"\n"^s3^": "^n3^"\n"
    ^s4^": "^n4^"\n"^s5^": "^n5^"\n"^s6^": "^n6
  in
  let xml = 
    if b then List.fold_left (
          fun s (c, n) ->
            s^"<stats type=\"Statements\" class=\""^c^"\" val=\""^n^"\"></stats>\n"
        ) "" [(s1, n1) ; (s2, n2) ; (s3, n3) ; (s4, n4) ; (s5, n5) ; (s6, n6)]
    else ""
  in
    out, xml

class collector ptr_sz fun_to_count =
object (this)
  inherit Lowspeak.visitor
    
  val mutable globals 	       = 0
  val mutable bytes 	       = 0
  val mutable void_fun 	       = 0

  val counters 		       = init_counters ()
  val funstats 		       = Hashtbl.create 10
  val globstats 	       = Hashtbl.create 10
  val mutable current_counters = init_counters ()
  val callstats 	       = Hashtbl.create 10
 
  method count_call f =
    try
      let nb_of_calls = Hashtbl.find callstats f in
        Hashtbl.replace callstats f (nb_of_calls + 1)
    with Not_found -> ()

  method incr_bytes i = 
    assert (i < max_int - bytes);
    bytes <- bytes + i

  method process_unop x =
    match x with
        N.Belongs _ -> current_counters.array <- current_counters.array + 1
      | _ -> ()

  method process_binop x =
    match x with
        N.PlusPI -> 
          current_counters.pointer_arith <- current_counters.pointer_arith + 1
      | _ -> ()

  method process_lval x =
    let _ =
      match x with
          Deref _ -> 
            current_counters.pointer_deref <- current_counters.pointer_deref + 1
        | _ -> ()
    in
      true

  method process_funexp x =
    let _ =
      match x with
          FunId f -> this#count_call f
        | FunDeref _ -> 
            current_counters.fpointer <- current_counters.fpointer + 1
    in
      true

  method process_stmt (x, _) =
   current_counters.instrs <- current_counters.instrs + 1;
    let _ = 
      match x with
          InfLoop _ -> 
            current_counters.loop <- current_counters.loop + 1
        | _ -> ()
    in
      true

  method process_fun f fdec =
    Hashtbl.add funstats f current_counters;
    if !more_verb then 
      match fdec.ftyp with
          ([], []) -> void_fun <- void_fun + 1
        | _, _ -> ()
    else ();
    true

  method process_fun_after () =
    incr_counters counters current_counters;
    current_counters <- init_counters ()

  method process_gdecl _ t =
    globals <- globals + 1;
    if !more_verb then 
      try 
        let n = Hashtbl.find globstats t in
          Hashtbl.replace globstats t (n+1)
      with
          Not_found -> Hashtbl.add globstats t 1
    else ();
    this#incr_bytes ((N.size_of ptr_sz t) / 8);
    true

  method gen_graphs fname =
    let cout = open_out (fname^".csv") in
    let cnt = ref 0 in
    let dump f counters =
      incr cnt;
      let f = if !obfuscate then "f"^(string_of_int !cnt) else f in
        output_string cout (f^"; ");
        output_string cout ((string_of_int counters.instrs)^"; ");
        output_string cout ((string_of_int counters.loop)^"; ");
        output_string cout ((string_of_int counters.array)^"; ");
        output_string cout ((string_of_int counters.pointer_deref)^"; ");
        output_string cout ((string_of_int counters.pointer_arith)^"; ");
        output_string cout ((string_of_int counters.fpointer)^"\n")
    in
      output_string cout "name; number of instructions; number of loops; ";
      output_string cout "number of array accesses; ";
      output_string cout "number of pointer dereference; ";
      output_string cout "number of pointer arithmetic; ";
      output_string cout "number of function pointer\n";
      Hashtbl.iter dump funstats;
      close_out cout


  method to_string verbose cout = 
    let res = Buffer.create 100 in
    let fun_counter = ref 0 in
    let b, cout = 
      match cout with 
          None -> false, stdout
        | Some cout -> true, cout 
    in
    let string_of_call f x =
      let n = string_of_int x in
      let s = "Number of calls to " in
      let out = "\n"^s^f^": "^n in
        Buffer.add_string res out;
        if b then
          "<stats type=\"Functions\" class=\""^s^"\" val=\""^n^"\"></stats>\n"
        else ""
          
    in
    
    let string_of_fun f counters =
      let f = if !obfuscate then string_of_int !fun_counter else f in
      let out, xml = string_of_counters counters b in
      let s = "Functions" in
        incr fun_counter;
        let out = "\n"^s^": "^f^"\n"^out in
          Buffer.add_string res out;
        if b then
          "<stats class=\""^s^"\" val=\""^f^"\"></stats>\n"^xml
        else ""

    in
    let string_of_globals globstats =
      let to_string (typ, nb) =
        let t = N.string_of_typ typ in
        let n = string_of_int nb in
          (t^": "^n^"\n"),
        ("<global type=\""^t^"\" nb=\""^n^"\"></global>\n")
      in
      let array_to_string ((typ, sz), nb) =
        let t = N.string_of_typ typ in
        let n1 = string_of_int sz in
        let n2 = string_of_int nb in
          (t^", "^n1^": "^n2^"\n"),
          ("<array type=\""^t^" " ^n1^"\" nb=\""^n2^"\"></array>\n")
      in
      let add_array l typ nb =
        match typ with
            N.Array t -> (t, nb)::l
          | _ -> l
      in
      let build_lists t n (l, ltab) =
        let ltab = add_array ltab t n in
          (t, n)::l, ltab
      in
      let l, ltab = Hashtbl.fold build_lists globstats ([], []) in
      let l = List.sort (fun v1 v2 -> (snd v2) - (snd v1)) l in
      let ltab = List.sort (fun v1 v2 -> (snd(fst(v2)) - snd(fst(v1)))) ltab in
      let out = "Number of globals with a given type: \n" in
      let out, xml = List.fold_left ( 
        fun (out, xml) e ->
          let o, x = to_string e in
            out^o, xml^x) (out, "") l in
      let out = 
	out ^ "Number of occurences of a given pair (array type, size):\n" 
      in
        List.fold_left (
          fun (out, xml) e ->
            let o, x = array_to_string e in
              out^o, xml^x) (out, xml) ltab
    in
    let s1 = "Number of global variables" in
    let n1 = string_of_int globals in
    let s2 = "Total size of global variables (bytes)" in
    let n2 = string_of_int bytes in
    let s3 = "Number of functions" in
    let n3 = string_of_int (Hashtbl.length funstats) in
      Buffer.add_string res (s1^": "^n1^"\n"^s2^": "^n2^"\n"^s3^": "^n3^"\n");
      if b then begin
        let xml = List.fold_left (
          fun s (t, c, n) ->
            s^"<stats type=\""^t^"\" class=\""^c^"\" val=\""^n^"\"></stats>\n"
        ) "" [("Global variables", s1, n1) ; ("Global variables", s2, n2) ; ("Functions", s3, n3)]
        in
          output_string cout xml
      end;
      if !more_verb then
        begin
          let out, xml = string_of_globals globstats in
          let s = "Number of functions with (void -> void) prototype: "  in
          let n = string_of_int void_fun in
            Buffer.add_string res (out^"\n");
            Buffer.add_string res (s^n^"\n");
            if b then begin
              output_string cout xml;
              output_string cout ("<void class=\""^s^"\" val=\""^n^"\"></void>\n")
            end
        end;
      let out, xml = string_of_counters counters b in
        Buffer.add_string res out;
        if xml <> "" then output_string cout xml;
        let xml =
          Hashtbl.fold (fun f x s ->
                          s^(string_of_call f x)) callstats ""
        in
          if xml <> "" then output_string cout xml;
      if verbose then begin
        let xml = Hashtbl.fold (fun f x s->
                                s^(string_of_fun f x)) funstats "" in
          if xml = "" then ()
          else output_string cout xml
      end;
        Buffer.contents res

  initializer List.iter (fun f -> Hashtbl.add callstats f 0) fun_to_count
end

let process input =
  let prog = Npk2lpk.translate (Newspeak.read input) in
  let collector = new collector prog.ptr_sz !fun_to_count in
  let max_stats = Maxcount.count !debug prog in
    Lowspeak.visit (collector :> Lowspeak.visitor) prog;
    let cout = 
      if !xout <> "" then begin
        let dtd = "<!-- <!DOCTYPE npkstats [\n" ^
          "<! ELEMENT npkstats (stats global void array) ?>\n" ^ 
          "<! ELEMENT stats ?>\n" ^
          "<! ELEMENT global ?>\n" ^
          "<! ATTLIST stats type (#PCDATA) #REQUIRED "^
          "class (#PCDATA) #REQUIRED "^
          "val (#PCDATA) #REQUIRED ?>\n" ^
          "<! ATTLIST global type (#PCDATA) #REQUIRED "^
          "nb (#PCDATA #REQUIRED ?>\n" ^
          "<! ATTLIST array type (#PCDATA) #REQUIRED "^
          "nb (#PCDATA #REQUIRED ?>\n" ^
          "<! ATTLIST void class (#PCDATA) #REQUIRED "^
          "val (#PCDATA) #REQUIRED >\n" ^
          "]> -->\n" in
        let header = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" in
        let cout = 
	  open_out_gen [Open_wronly;Open_binary;Open_creat] 0o644 !xout 
	in
          output_string cout (header^dtd^"<npkstats>\n");
          Some cout
      end
      else None
    in
      print_endline (collector#to_string !verbose cout);
      Maxcount.print max_stats cout;
      Libfuns.collect prog cout;
      begin
        match cout with 
            None -> ()
          | Some cout ->
              output_string cout "</npkstats>\n";
              close_out cout
      end;
      if !graphs then collector#gen_graphs !output;
      if !funstats then Funstats.collect prog

let _ = 
  StandardApplication.launch_process_with_npk_argument "npkstats" speclist 
    process
      
