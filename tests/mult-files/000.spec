Newspeak output
---------------
000-a.c
000-b.c
void f(void) {
  (000-b.c:28#2)^[y_ptr]8 =(int8) 1;
}

void main(void) {
  (000-a.c:28#2)^[x_ptr]8 =(int8) 0;
}

int8[6] !000-a.c.const_str_Hello;
int8[6] !000-b.c.const_str_Hello;
ptr x;
ptr y;
(000-b.c:25#6)^!000-b.c.const_str_Hello =(int8) 72;
(000-b.c:25#6)^!000-b.c.const_str_Hello + 8 =(int8) 101;
(000-b.c:25#6)^!000-b.c.const_str_Hello + 16 =(int8) 108;
(000-b.c:25#6)^!000-b.c.const_str_Hello + 24 =(int8) 108;
(000-b.c:25#6)^!000-b.c.const_str_Hello + 32 =(int8) 111;
(000-b.c:25#6)^!000-b.c.const_str_Hello + 40 =(int8) 0;
(000-b.c:25#6)^y =(ptr) &_48(!000-b.c.const_str_Hello);
(000-a.c:25#6)^x =(ptr) &_48(!000-a.c.const_str_Hello);
(000-a.c:25#6)^!000-a.c.const_str_Hello =(int8) 72;
(000-a.c:25#6)^!000-a.c.const_str_Hello + 8 =(int8) 101;
(000-a.c:25#6)^!000-a.c.const_str_Hello + 16 =(int8) 108;
(000-a.c:25#6)^!000-a.c.const_str_Hello + 24 =(int8) 108;
(000-a.c:25#6)^!000-a.c.const_str_Hello + 32 =(int8) 111;
(000-a.c:25#6)^!000-a.c.const_str_Hello + 40 =(int8) 0;

