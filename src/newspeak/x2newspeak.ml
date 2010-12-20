
(* TODO: should have a structure with the version and comment string 
   instead *)
let process version_string comment_string execute =
  try
    Npkcontext.handle_cmdline_options version_string comment_string;
    execute ()
  with Invalid_argument msg ->
    prerr_endline ("Fatal error: "^msg);
    exit 1
