
(* Producer for the test anything protocol. *)
(* Not reentrant. *)

val test_plan : int -> unit

val test_end : unit -> unit

val test_ok : string -> unit

val assert_equal : string -> int -> int -> unit

val assert_true : string -> bool -> unit

val assert_false : string -> bool -> unit

val assert_equal_string : string -> string -> string -> unit
