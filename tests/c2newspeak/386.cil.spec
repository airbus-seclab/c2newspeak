Newspeak output
---------------
386.c
void main(void) {
  (386.c:27#1073)^ptr t;
  (386.c:28#1106)^ptr u;
  (386.c:27#1068)^1- =(ptr) &_96(!386.c.const_str_Hello world);
  (386.c:28#1101)^0- =(ptr) &_96(!386.c.const_str_Hello world);
  (386.c:29#1128)^[1-_ptr]8 =(int8) [0-_ptr]8_int8;
}

int8[12] !386.c.const_str_Hello world;
(386.c)^!386.c.const_str_Hello world =(int8) 72;
(386.c)^!386.c.const_str_Hello world + 8 =(int8) 101;
(386.c)^!386.c.const_str_Hello world + 16 =(int8) 108;
(386.c)^!386.c.const_str_Hello world + 24 =(int8) 108;
(386.c)^!386.c.const_str_Hello world + 32 =(int8) 111;
(386.c)^!386.c.const_str_Hello world + 40 =(int8) 32;
(386.c)^!386.c.const_str_Hello world + 48 =(int8) 119;
(386.c)^!386.c.const_str_Hello world + 56 =(int8) 111;
(386.c)^!386.c.const_str_Hello world + 64 =(int8) 114;
(386.c)^!386.c.const_str_Hello world + 72 =(int8) 108;
(386.c)^!386.c.const_str_Hello world + 80 =(int8) 100;
(386.c)^!386.c.const_str_Hello world + 88 =(int8) 0;

