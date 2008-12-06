Warning: 007.c:34#2: dirty cast from pointer to integer accepted
Newspeak output
---------------
007.c
void main(void) {
  (007.c:32#7)^ptr p;
  (007.c:33#6)^int32 x;
  (007.c:34#2)^0- =(int32) (int32) 1-_ptr;
}


