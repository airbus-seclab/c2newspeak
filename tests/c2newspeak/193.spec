Newspeak output
---------------
void (193.c:28#5)^main(void) {
  (193.c:29#2)^x =(ptr) y_ptr;
}

int8[6] !cstr.Hello;
ptr x;
ptr y;
(193.c:25#6)^!cstr.Hello =(int8) 72;
(193.c:25#6)^!cstr.Hello + 8 =(int8) 101;
(193.c:25#6)^!cstr.Hello + 16 =(int8) 108;
(193.c:25#6)^!cstr.Hello + 24 =(int8) 108;
(193.c:25#6)^!cstr.Hello + 32 =(int8) 111;
(193.c:25#6)^!cstr.Hello + 40 =(int8) 0;
(193.c:25#6)^x =(ptr) focus48 &(!cstr.Hello);
(193.c:26#6)^y =(ptr) focus48 &(!cstr.Hello);

