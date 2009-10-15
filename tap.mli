
(* Producer for the test anything protocol. *)
(* Not reentrant. *)

val test_plan : int -> unit

val test_end : unit -> unit

val test_ok : string -> unit

val assert_equal : ?printer:('a -> string) -> 'a -> 'a -> string -> unit

val assert_equal_int : int -> int -> string -> unit

val assert_true  : bool -> string -> unit
                                    
val assert_false : bool -> string -> unit

val assert_equal_string : string -> string -> string -> unit
