Newspeak output
---------------
062.c
int32 f(int32 a) {
  (062.c:27#2)^!return =(int32) a_int32;
}

void main(void) {
  (062.c:31#6)^int32 x;
  (062.c:32#2)^int32 tmp_cir!0;
  (062.c:32#2)^tmp_cir!0 <- f(1);
  (062.c:32#2)^x <- f(tmp_cir!0_int32);
}


