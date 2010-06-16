Warning: 418.c:29#2: dirty cast from pointer to integer accepted
Newspeak output
---------------
418.c
void main(void) {
  (418.c:27#7)^ptr ptr;
  (418.c:28#6)^int32 x;
  (418.c:29#2)^x =(int32) (int32) ptr_ptr;
}


