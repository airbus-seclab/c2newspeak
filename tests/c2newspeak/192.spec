Newspeak output
---------------
void (test53-a.c:2#5)^main(void) {
  (test53-a.c:3#7)^int8 x;
  (test53-a.c:4#2)^x =(int8) [ptr_ptr]8_int8;
}

int8[6] cstr!0!.Hello;
ptr ptr;
(test53-b.c:1#6)^cstr!0!.Hello =(int8) 72;
(test53-b.c:1#6)^cstr!0!.Hello + 8 =(int8) 101;
(test53-b.c:1#6)^cstr!0!.Hello + 16 =(int8) 108;
(test53-b.c:1#6)^cstr!0!.Hello + 24 =(int8) 108;
(test53-b.c:1#6)^cstr!0!.Hello + 32 =(int8) 111;
(test53-b.c:1#6)^cstr!0!.Hello + 40 =(int8) 0;
(test53-b.c:1#6)^ptr =(ptr) focus48 &(cstr!0!.Hello);

