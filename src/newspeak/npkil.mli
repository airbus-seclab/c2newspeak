open Npkutils

type glb_type = {
  mutable gtype : Cil.typ;
  mutable gloc  : Newspeak.location;
  mutable gdefd : bool;
  mutable ginit : Cil.init option;
}

type fspec_type = {
  mutable prett : Newspeak.typ option;
  mutable pargs : ((int * string * Newspeak.typ) list) option;
  mutable plocs : ((int * string * Newspeak.typ) list) option;
  mutable ploc  : Newspeak.location;
  mutable pbody : Newspeak.blk option;
  mutable pcil_body : Cil.block option
}

type intermediate = {
  ifilename : string;
  iglobs : (string, glb_type) Hashtbl.t;
  ifuns  : (Newspeak.fid, fspec_type) Hashtbl.t;
  iusedglbs : String_set.t;
  iusedcstr : String_set.t;
  iusedfuns : String_set.t;
}


val dump_npko : intermediate -> unit

