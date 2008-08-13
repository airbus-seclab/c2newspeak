Warning: Probable invalid cast from integer to pointer in 235.c line 31
Newspeak output
---------------
235.c
main() {
  (235.c:27#1072)^int32 x;
  (235.c:28#1082)^ptr ptr;
  (235.c:29#1093)^int32 y;
  (235.c:31#1100)^0- =(int32) ((ptr) 2-_int32 > 1-_ptr);
}


