Newspeak output
---------------
114.c
void main(void) {
  (114.c:31#1096)^int32 x;
  (114.c:32#1110)^ptr ptr;
  ptr tmp;
  (114.c:33#1118)^tmp =(ptr) ptr_ptr;
  (114.c:33#1118)^ptr =(ptr) (ptr_ptr + 32);
  (114.c:33#1118)^x =(int32) [tmp_ptr]32_int32;
}


