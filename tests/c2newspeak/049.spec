Newspeak output
---------------
049.c
void main(void) {
  (049.c:32#7)^int8 c;
  (049.c:33#2)^0- =(int8) [x_ptr]8_int8;
}

int8[6] !049.c.const_str_Hello;
ptr x;
(049.c:29#6)^x =(ptr) &_48(!049.c.const_str_Hello);
(049.c:29#6)^!049.c.const_str_Hello =(int8) 72;
(049.c:29#6)^!049.c.const_str_Hello + 8 =(int8) 101;
(049.c:29#6)^!049.c.const_str_Hello + 16 =(int8) 108;
(049.c:29#6)^!049.c.const_str_Hello + 24 =(int8) 108;
(049.c:29#6)^!049.c.const_str_Hello + 32 =(int8) 111;
(049.c:29#6)^!049.c.const_str_Hello + 40 =(int8) 0;

