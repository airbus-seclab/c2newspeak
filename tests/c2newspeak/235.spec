Warning: 235.c:31#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
235.c
void main(void) {
  (235.c:27#6)^int32 x;
  (235.c:28#7)^ptr ptr;
  (235.c:29#6)^int32 y;
  (235.c:31#2)^0- =(int32) ((ptr) 2-_int32 > 1-_ptr);
}


