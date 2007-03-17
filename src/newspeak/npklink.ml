open Npkcontext
open Env

let link npkos out_name =
  print_debug "Linking files...";
  (* TODO: Do something ;-) *)
  print_debug "File linked.";

  if !verb_newspeak then begin
    print_endline "Newspeak output";
    print_endline "---------------";
    (* TODO: Dump something *)
    (* K.dump kernel; *)
    print_newline ()
  end;

  print_debug ("Writing output to "^out_name^"...");
  let ch_out = open_out_bin out_name in
    Marshal.to_channel ch_out "NPK!" [];
    (* TODO: Write something ;-) *)
    (* Marshal.to_channel ch_out (fnames, kernel) []; *)
    close_out ch_out;
    print_debug (out_name^" written.")
