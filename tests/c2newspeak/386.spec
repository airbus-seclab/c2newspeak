Newspeak output
---------------
void (386.c:26#5)^main(void) {
  (386.c:27#8)^ptr t;
  (386.c:27#8)^t =(ptr) focus96 &(!cstr.Hello world);
  (386.c:28#8)^{
    ptr u;
    (386.c:28#8)^u =(ptr) focus96 &(!cstr.Hello world);
    (386.c:29#2)^[t_ptr]8 =(int8) [u_ptr]8_int8;
  }
}

int8[12] !cstr.Hello world;
(386.c:27#8)^!cstr.Hello world =(int8) 72;
(386.c:27#8)^!cstr.Hello world + 8 =(int8) 101;
(386.c:27#8)^!cstr.Hello world + 16 =(int8) 108;
(386.c:27#8)^!cstr.Hello world + 24 =(int8) 108;
(386.c:27#8)^!cstr.Hello world + 32 =(int8) 111;
(386.c:27#8)^!cstr.Hello world + 40 =(int8) 32;
(386.c:27#8)^!cstr.Hello world + 48 =(int8) 119;
(386.c:27#8)^!cstr.Hello world + 56 =(int8) 111;
(386.c:27#8)^!cstr.Hello world + 64 =(int8) 114;
(386.c:27#8)^!cstr.Hello world + 72 =(int8) 108;
(386.c:27#8)^!cstr.Hello world + 80 =(int8) 100;
(386.c:27#8)^!cstr.Hello world + 88 =(int8) 0;

