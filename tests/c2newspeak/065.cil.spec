Newspeak output
---------------
065.c
int32 f(int32 a) {
  (065.c:27#1069)^!return =(int32) a_int32;
}

void main(void) {
  (065.c:31#1102)^int32 x;
  int32 tmp;
  int32 tmp___0;
  (065.c:32#1107)^tmp <- f(2);
  (065.c:32#1107)^tmp___0 <- f(3);
  (065.c:32#1107)^x =(int32) coerce[-2147483648,2147483647] (tmp_int32 + tmp___0_int32);
}


