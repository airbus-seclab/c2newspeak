Newspeak output
---------------
int32 (219.c:26#4)^f(void) {
  (219.c:27#2)^!return =(int32) 0;
}

void (219.c:30#5)^main(void) {
  (219.c:31#6)^int32 x;
  (219.c:32#2)^{
    int32 tmp_cir!0;
    (219.c:32#2)^tmp_cir!0: int32 <- f();
    (219.c:32#2)^x =(int32) coerce[-2147483648,2147483647] (tmp_cir!0_int32 + 1);
  }
  (219.c:33#2)^x =(int32) 2;
}


