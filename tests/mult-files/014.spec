Newspeak output
---------------
void (014-a.c:5#5)^f(void) {
  (014-a.c:7#8)^ptr x;
  (014-a.c:8#2)^x =(ptr) focus48 &(cstr!0!.hello);
}

void (014-b.c:1#5)^h(void) {
  (014-b.c:3#8)^ptr y;
  (014-b.c:4#2)^y =(ptr) focus48 &(cstr!0!.hello);
}

int8[6] cstr!0!.hello;
(014-a.c:8#2)^cstr!0!.hello =(int8) 104;
(014-a.c:8#2)^cstr!0!.hello + 8 =(int8) 101;
(014-a.c:8#2)^cstr!0!.hello + 16 =(int8) 108;
(014-a.c:8#2)^cstr!0!.hello + 24 =(int8) 108;
(014-a.c:8#2)^cstr!0!.hello + 32 =(int8) 111;
(014-a.c:8#2)^cstr!0!.hello + 40 =(int8) 0;

