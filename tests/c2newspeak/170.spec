Warning: 170.c:31#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
170.c
void main(void) {
  (170.c:29#7)^ptr x;
  (170.c:31#2)^uint32 tmp_cir!0;
  (170.c:31#2)^tmp_cir!0: uint32 <- f();
  (170.c:31#2)^x =(ptr) (ptr) tmp_cir!0_uint32;
}


