Warning: 254.c:28#2: expression without post effects expected
Newspeak output
---------------
void main(void) {
  (254.c:27#6)^int32 x;
  (254.c:28#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (254.c:28#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
}


