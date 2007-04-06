(*-----------------*)
(* File extensions *)
(*-----------------*)

let c_suffix = ".c"
let npko_suffix = ".no"



(*------------------------*)
(* Version and other info *)
(*------------------------*)

let software = "C2Newspeak"
let version = "0.8"
let authors = "Olivier Levillain and Charles Hymans"
let licence = "LGPL v. ??"
let copyright = "EADS"
let comment =[
  "   The Newspeak language and C2Newspeak have initially been developped in";
  " EADS Innovation Works, Suresnes, France, by Olivier Levillain and Charles";
  " Hymans.";
  "";
  "   C2Newspeak compiles C code into Newspeak language, which allows simple";
  " analysis and statistics on C code thanks to a simpler and non ambiguous";
  " language."]


let version_string =
  software^" v."^version^" by "^authors^".\nSoftware under "
  ^licence^". Copyright "^copyright^".\n"


let print_version () =
  print_endline (version_string);
  List.iter print_endline comment
