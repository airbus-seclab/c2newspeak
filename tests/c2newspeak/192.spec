Newspeak output
---------------
test53-a.c
test53-b.c
void main(void) {
  (test53-a.c:3#7)^int8 x;
  (test53-a.c:4#2)^0- =(int8) [ptr_ptr]8_int8;
}

int8[6] !192.c.const_str_Hello;
ptr ptr;
(test53-b.c:1#6)^ptr =(ptr) focus48 &(!192.c.const_str_Hello);
(test53-b.c:1#6)^!192.c.const_str_Hello =(int8) 72;
(test53-b.c:1#6)^!192.c.const_str_Hello + 8 =(int8) 101;
(test53-b.c:1#6)^!192.c.const_str_Hello + 16 =(int8) 108;
(test53-b.c:1#6)^!192.c.const_str_Hello + 24 =(int8) 108;
(test53-b.c:1#6)^!192.c.const_str_Hello + 32 =(int8) 111;
(test53-b.c:1#6)^!192.c.const_str_Hello + 40 =(int8) 0;

