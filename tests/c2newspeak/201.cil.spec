Newspeak output
---------------
201.c
void main(void) {
  (201.c:27#1072)^int32 x;
  (201.c:28#1081)^int32 y;
  int32 tmp;
  (201.c:29#1086)^tmp =(int32) x_int32;
  (201.c:29#1086)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (201.c:29#1086)^y =(int32) tmp_int32;
}


