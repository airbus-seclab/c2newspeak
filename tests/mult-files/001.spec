Newspeak output
---------------
001-a.c
001-b.c
void main(void) {
  (001-a.c:28#2)^[x_ptr]8 =(int8) 0;
}

int8[6] !001-b.c.const_str_Hello;
ptr x;
(001-b.c:25#6)^x =(ptr) &_48(!001-b.c.const_str_Hello);
(001-b.c:25#6)^!001-b.c.const_str_Hello =(int8) 72;
(001-b.c:25#6)^!001-b.c.const_str_Hello + 8 =(int8) 101;
(001-b.c:25#6)^!001-b.c.const_str_Hello + 16 =(int8) 108;
(001-b.c:25#6)^!001-b.c.const_str_Hello + 24 =(int8) 108;
(001-b.c:25#6)^!001-b.c.const_str_Hello + 32 =(int8) 111;
(001-b.c:25#6)^!001-b.c.const_str_Hello + 40 =(int8) 0;

