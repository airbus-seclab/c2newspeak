Warning: dirty cast from integer to pointer accepted in 467.c line 33
Newspeak output
---------------
467.c
main() {
  (467.c:32#6)^int32 x;
  (467.c:33#2)^ptr f.arg1;
  (467.c:33#2)^0- =(ptr) (ptr) 1-_int32;
  (467.c:33#2)^f();
}


