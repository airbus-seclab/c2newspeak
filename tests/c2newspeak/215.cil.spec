Newspeak output
---------------
215.c
int32 f(void) {
  (215.c:30#1100)^!return =(int32) 4;
}

void main(void) {
  (215.c:34#1133)^int32[10] t;
  int32 tmp;
  (215.c:36#1166)^tmp =(int32) x_int32;
  (215.c:36#1166)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (215.c:36#1166)^t + (belongs[0,9] tmp_int32 * 32) <- f();
}

int32 x;

