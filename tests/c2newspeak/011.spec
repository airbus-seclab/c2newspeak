Newspeak output
---------------
int32 f(int32 a, int32 b) {
  (011.c:33#2)^!return =(int32) coerce[-2147483648,2147483647] (a_int32 + b_int32);
}

void main(void) {
  (011.c:37#6)^int32 x;
  (011.c:37#9)^int32 y;
  (011.c:37#12)^int32 z;
  (011.c:38#2)^z: int32 <- f(x_int32: int32, y_int32: int32);
}


