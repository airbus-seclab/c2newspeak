type intermediate = {
  ifilename : string;
  iglobs : (string, Env.glb_type) Hashtbl.t;
  ifuns  : (Newspeak.fid, Env.fspec_type) Hashtbl.t;
  iusedglbs : Npkutils.String_set.t;
  iusedcstr : Npkutils.String_set.t;
  iusedfuns : Npkutils.String_set.t;
}


val dump_npko : intermediate -> unit

