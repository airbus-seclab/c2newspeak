Newspeak output
---------------
193.c
void main(void) {
  (193.c:29#1107)^x =(ptr) y_ptr;
}

int8[6] !193.c.const_str_Hello;
ptr x;
ptr y;
(193.c)^!193.c.const_str_Hello =(int8) 72;
(193.c)^!193.c.const_str_Hello + 8 =(int8) 101;
(193.c)^!193.c.const_str_Hello + 16 =(int8) 108;
(193.c)^!193.c.const_str_Hello + 24 =(int8) 108;
(193.c)^!193.c.const_str_Hello + 32 =(int8) 111;
(193.c)^!193.c.const_str_Hello + 40 =(int8) 0;
(193.c:26#1075)^y =(ptr) &_48(!193.c.const_str_Hello);
(193.c:25#1056)^x =(ptr) &_48(!193.c.const_str_Hello);

