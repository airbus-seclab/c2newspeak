Newspeak output
---------------
011.c
int32 f(int32 a, int32 b) {
  (011.c:33#1269)^!return =(int32) coerce[-2147483648,2147483647] (a_int32 + b_int32);
}

void main(void) {
  (011.c:37#1306)^int32 x;
  (011.c:37#1309)^int32 y;
  (011.c:37#1312)^int32 z;
  (011.c:38#1317)^z <- f(x_int32, y_int32);
}


