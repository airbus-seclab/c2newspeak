Warning: 006.c:34#2: dirty cast from pointer to integer accepted
Newspeak output
---------------
void (006.c:31#5)^main(void) {
  (006.c:32#7)^ptr p;
  (006.c:33#6)^int32 x;
  (006.c:34#2)^x =(int32) (int32) p_ptr;
}


