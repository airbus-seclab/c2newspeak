Newspeak output
---------------
210.c
void f(int32 x) {
}

void main(void) {
  (210.c:30#1091)^int32 x;
  int32 tmp;
  (210.c:32#1097)^tmp =(int32) x_int32;
  (210.c:32#1097)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (210.c:32#1097)^f(tmp_int32);
}


