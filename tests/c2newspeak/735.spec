Newspeak output
---------------
int32 (735.c:26#4)^f(void) {
  (735.c:27#2)^!return =(int32) 1;
}

void (735.c:30#5)^main(void) {
  (735.c:31#6)^int32 x;
  (735.c:32#2)^int32 tmp_cir!0;
  (735.c:32#2)^tmp_cir!0: int32 <- f();
  (735.c:32#2)^x =(int32) coerce[-2147483648,2147483647] (1 + tmp_cir!0_int32);
}


