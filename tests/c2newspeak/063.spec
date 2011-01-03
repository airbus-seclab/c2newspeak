Newspeak output
---------------
int32 (063.c:26#4)^f(int32 a) {
  (063.c:27#2)^!return =(int32) a_int32;
}

void (063.c:30#5)^main(void) {
  (063.c:31#6)^int32 x;
  (063.c:32#2)^{
    int32 tmp_cir!0;
    (063.c:32#2)^tmp_cir!0: int32 <- f(1: int32);
    (063.c:32#2)^x: int32 <- f(tmp_cir!0_int32: int32);
  }
  (063.c:34#8)^{
    int32 y;
    (063.c:35#4)^x =(int32) y_int32;
  }
}


