Warning: probable invalid cast from integer to pointer in 170.c line 31
Newspeak output
---------------
170.c
main() {
  (170.c:29#1107)^ptr x;
  (170.c:31#1113)^uint32 !tmp-1073741822;
  (170.c:31#1113)^{
    uint32 value_of_f;
    (170.c:31#1113)^f();
    (170.c:31#1113)^1- =(uint32) 0-_uint32;
  }
  (170.c:31#1113)^1- =(ptr) (ptr) 0-_uint32;
}


