Warning: 235.c:31#1100: dirty cast from pointer to integer accepted
Newspeak output
---------------
235.c
main() {
  (235.c:27#1072)^int32 x;
  (235.c:28#1081)^ptr ptr;
  (235.c:29#1093)^int32 y;
  (235.c:31#1100)^0- =(int32) (coerce[0,4294967295] 2-_int32 > (uint32) 1-_ptr);
}


