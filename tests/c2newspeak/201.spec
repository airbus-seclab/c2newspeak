Newspeak output
---------------
201.c
void main(void) {
  (201.c:27#6)^int32 x;
  (201.c:28#6)^int32 y;
  (201.c:29#2)^y =(int32) x_int32;
  (201.c:29#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
}


