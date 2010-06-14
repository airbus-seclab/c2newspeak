Newspeak output
---------------
214.c
void f(int32 x, int32 y) {
}

void main(void) {
  (214.c:31#1126)^int32 x;
  int32 tmp;
  int32 tmp___0;
  (214.c:31#1122)^x =(int32) 0;
  (214.c:32#1135)^tmp =(int32) x_int32;
  (214.c:32#1135)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (214.c:32#1135)^tmp___0 =(int32) x_int32;
  (214.c:32#1135)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (214.c:32#1135)^f(tmp___0_int32, tmp_int32);
}


