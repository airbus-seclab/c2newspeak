Newspeak output
---------------
void (210.c:26#5)^f(int32 x) {
}

void (210.c:29#5)^main(void) {
  (210.c:30#6)^int32 x;
  (210.c:32#2)^int32 tmp_cir!0;
  (210.c:32#2)^tmp_cir!0 =(int32) x_int32;
  (210.c:32#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (210.c:32#2)^f(tmp_cir!0_int32: int32);
}


