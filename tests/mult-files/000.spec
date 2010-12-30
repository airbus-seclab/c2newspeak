Newspeak output
---------------
void (000-b.c:27#5)^f(void) {
  (000-b.c:28#2)^[y_ptr]8 =(int8) 1;
}

void (000-a.c:27#5)^main(void) {
  (000-a.c:28#2)^[x_ptr]8 =(int8) 0;
}

int8[6] cstr!0!.Hello;
ptr x;
ptr y;
(000-a.c:25#6)^cstr!0!.Hello =(int8) 72;
(000-a.c:25#6)^cstr!0!.Hello + 8 =(int8) 101;
(000-a.c:25#6)^cstr!0!.Hello + 16 =(int8) 108;
(000-a.c:25#6)^cstr!0!.Hello + 24 =(int8) 108;
(000-a.c:25#6)^cstr!0!.Hello + 32 =(int8) 111;
(000-a.c:25#6)^cstr!0!.Hello + 40 =(int8) 0;
(000-a.c:25#6)^x =(ptr) focus48 &(cstr!0!.Hello);
(000-b.c:25#6)^y =(ptr) focus48 &(cstr!0!.Hello);

