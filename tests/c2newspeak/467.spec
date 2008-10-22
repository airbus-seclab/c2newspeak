Warning: dirty cast from integer to pointer accepted in 467.c line 33
Newspeak output
---------------
467.c
main() {
  (467.c:32#1155)^int32 x;
  (467.c:33#1160)^ptr f.arg1;
  (467.c:33#1160)^0- =(ptr) (ptr) 1-_int32;
  (467.c:33#1160)^f();
}


