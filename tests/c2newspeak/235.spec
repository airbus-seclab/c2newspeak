Warning: 235.c:31#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
void (235.c:26#5)^main(void) {
  (235.c:27#6)^int32 x;
  (235.c:28#7)^ptr ptr;
  (235.c:29#6)^int32 y;
  (235.c:31#2)^y =(int32) ((ptr) x_int32 > ptr_ptr);
}


