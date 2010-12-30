Newspeak output
---------------
void (014-a.c:1#5)^f(void) {
  (014-a.c:3#8)^ptr x;
  (014-a.c:4#2)^x =(ptr) focus48 &(cstr!0!014-a.c.hello);
}

void (014-b.c:1#5)^h(void) {
  (014-b.c:3#8)^ptr y;
  (014-b.c:4#2)^y =(ptr) focus48 &(cstr!0!014-b.c.hello);
}

int8[6] cstr!0!014-a.c.hello;
int8[6] cstr!0!014-b.c.hello;
(014-a.c:4#2)^cstr!0!014-a.c.hello =(int8) 104;
(014-a.c:4#2)^cstr!0!014-a.c.hello + 8 =(int8) 101;
(014-a.c:4#2)^cstr!0!014-a.c.hello + 16 =(int8) 108;
(014-a.c:4#2)^cstr!0!014-a.c.hello + 24 =(int8) 108;
(014-a.c:4#2)^cstr!0!014-a.c.hello + 32 =(int8) 111;
(014-a.c:4#2)^cstr!0!014-a.c.hello + 40 =(int8) 0;
(014-b.c:4#2)^cstr!0!014-b.c.hello =(int8) 104;
(014-b.c:4#2)^cstr!0!014-b.c.hello + 8 =(int8) 101;
(014-b.c:4#2)^cstr!0!014-b.c.hello + 16 =(int8) 108;
(014-b.c:4#2)^cstr!0!014-b.c.hello + 24 =(int8) 108;
(014-b.c:4#2)^cstr!0!014-b.c.hello + 32 =(int8) 111;
(014-b.c:4#2)^cstr!0!014-b.c.hello + 40 =(int8) 0;

