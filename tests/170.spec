Warning: Probable invalid cast from integer to pointer in 170.c line 31
Newspeak output
---------------
170.c
main() {
  (170.c:29#1107)^ptr;
  (170.c:31#1113)^uint32;
  (170.c:31#1113)^{
    uint32;
    (170.c:31#1113)^f();
    (170.c:31#1113)^1- =(uint32) 0-_uint32;
  }
  (170.c:31#1113)^1- =(ptr) (ptr) 0-_uint32;
}


