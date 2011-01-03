Newspeak output
---------------
void (049.c:31#5)^main(void) {
  (049.c:32#7)^int8 c;
  (049.c:33#2)^c =(int8) [x_ptr]8_int8;
}

int8[6] !cstr.Hello;
ptr x;
(049.c:29#6)^!cstr.Hello =(int8) 72;
(049.c:29#6)^!cstr.Hello + 8 =(int8) 101;
(049.c:29#6)^!cstr.Hello + 16 =(int8) 108;
(049.c:29#6)^!cstr.Hello + 24 =(int8) 108;
(049.c:29#6)^!cstr.Hello + 32 =(int8) 111;
(049.c:29#6)^!cstr.Hello + 40 =(int8) 0;
(049.c:29#6)^x =(ptr) focus48 &(!cstr.Hello);

