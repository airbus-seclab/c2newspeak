Newspeak output
---------------
int32 f(int32 a) {
  (062.c:27#2)^!return =(int32) a_int32;
}

void main(void) {
  (062.c:31#6)^int32 x;
  (062.c:32#2)^int32 tmp_cir!0;
  (062.c:32#2)^tmp_cir!0: int32 <- f(1: int32);
  (062.c:32#2)^x: int32 <- f(tmp_cir!0_int32: int32);
}


