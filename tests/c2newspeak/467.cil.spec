Warning: 467.c:33#1160: dirty cast from integer to pointer accepted
Newspeak output
---------------
467.c
void main(void) {
  (467.c:32#1155)^int32 x;
  (467.c:33#1160)^ptr arg0;
  (467.c:33#1160)^0- =(ptr) (ptr) 1-_int32;
  (467.c:33#1160)^f();
}


