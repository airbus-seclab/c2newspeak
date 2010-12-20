Newspeak output
---------------
void f(void) {
  (000-b.c:28#2)^[y_ptr]8 =(int8) 1;
}

void main(void) {
  (000-a.c:28#2)^[x_ptr]8 =(int8) 0;
}

int8[6] cstr!1!000-b.c.Hello;
int8[6] cstr!2!000-a.c.Hello;
ptr x;
ptr y;
(000-a.c:25#6)^x =(ptr) focus48 &(cstr!2!000-a.c.Hello);
(000-a.c:25#6)^cstr!2!000-a.c.Hello =(int8) 72;
(000-a.c:25#6)^cstr!2!000-a.c.Hello + 8 =(int8) 101;
(000-a.c:25#6)^cstr!2!000-a.c.Hello + 16 =(int8) 108;
(000-a.c:25#6)^cstr!2!000-a.c.Hello + 24 =(int8) 108;
(000-a.c:25#6)^cstr!2!000-a.c.Hello + 32 =(int8) 111;
(000-a.c:25#6)^cstr!2!000-a.c.Hello + 40 =(int8) 0;
(000-b.c:25#6)^y =(ptr) focus48 &(cstr!1!000-b.c.Hello);
(000-b.c:25#6)^cstr!1!000-b.c.Hello =(int8) 72;
(000-b.c:25#6)^cstr!1!000-b.c.Hello + 8 =(int8) 101;
(000-b.c:25#6)^cstr!1!000-b.c.Hello + 16 =(int8) 108;
(000-b.c:25#6)^cstr!1!000-b.c.Hello + 24 =(int8) 108;
(000-b.c:25#6)^cstr!1!000-b.c.Hello + 32 =(int8) 111;
(000-b.c:25#6)^cstr!1!000-b.c.Hello + 40 =(int8) 0;

