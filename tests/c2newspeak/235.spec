Warning: dirty cast from integer to pointer accepted in 235.c line 31
Newspeak output
---------------
235.c
main() {
  (235.c:27#6)^int32 x;
  (235.c:28#7)^ptr ptr;
  (235.c:29#6)^int32 y;
  (235.c:31#2)^0- =(int32) ((ptr) 2-_int32 > 1-_ptr);
}


