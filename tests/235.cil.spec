Warning: Probable invalid cast from pointer to integer in 235.c line 31
Newspeak output
---------------
235.c
main() {
  (235.c:27#1072)^int32;
  (235.c:28#1081)^ptr;
  (235.c:29#1093)^int32;
  (235.c:31#1100)^0- =(int32) (coerce[0,4294967295] 2-_int32 > (uint32) 1-_ptr);
}

