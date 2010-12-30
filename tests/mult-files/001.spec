Newspeak output
---------------
void (001-a.c:27#5)^main(void) {
  (001-a.c:28#2)^[x_ptr]8 =(int8) 0;
}

int8[6] !cstr.Hello;
ptr x;
(001-b.c:25#6)^!cstr.Hello =(int8) 72;
(001-b.c:25#6)^!cstr.Hello + 8 =(int8) 101;
(001-b.c:25#6)^!cstr.Hello + 16 =(int8) 108;
(001-b.c:25#6)^!cstr.Hello + 24 =(int8) 108;
(001-b.c:25#6)^!cstr.Hello + 32 =(int8) 111;
(001-b.c:25#6)^!cstr.Hello + 40 =(int8) 0;
(001-b.c:25#6)^x =(ptr) focus48 &(!cstr.Hello);

