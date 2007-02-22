type counter

val count: counter -> unit
val count_call: Newspeak.fid -> unit

val array: counter
val pointer_arith: counter
val pointer_deref: counter
val fpointer: counter
val loop: counter
val funct: counter

val to_string: unit -> string

open Arg 
val args: (key * spec * doc) list 
