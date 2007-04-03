open Npkutils

type intermediate = {
  ifilename : string;
  iglobs : (string, Npkenv.glb_type) Hashtbl.t;
  ifuns  : (Newspeak.fid, Npkenv.fspec_type) Hashtbl.t;
  iusedglbs : String_set.t;
  iusedcstr : String_set.t;
  iusedfuns : String_set.t;
}


val dump_npko : intermediate -> unit

