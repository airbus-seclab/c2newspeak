Newspeak output
---------------
208.c
void main(void) {
  (208.c:27#1072)^int32 x;
  int32 tmp;
  (208.c:29#1078)^tmp =(int32) x_int32;
  (208.c:29#1078)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (208.c:29#1078)^x =(int32) ~ tmp_int32;
}


