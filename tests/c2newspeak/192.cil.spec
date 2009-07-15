Newspeak output
---------------
192.c
void main(void) {
  (test53-a.c:3#1256)^int8 x;
  (test53-a.c:4#1261)^0- =(int8) [ptr_ptr]8_int8;
}

int8[6] !test53-b.c.const_str_Hello;
ptr ptr;
(test53-b.c)^!test53-b.c.const_str_Hello =(int8) 72;
(test53-b.c)^!test53-b.c.const_str_Hello + 8 =(int8) 101;
(test53-b.c)^!test53-b.c.const_str_Hello + 16 =(int8) 108;
(test53-b.c)^!test53-b.c.const_str_Hello + 24 =(int8) 108;
(test53-b.c)^!test53-b.c.const_str_Hello + 32 =(int8) 111;
(test53-b.c)^!test53-b.c.const_str_Hello + 40 =(int8) 0;
(test53-b.c:1#1128)^ptr =(ptr) &_48(!test53-b.c.const_str_Hello);

