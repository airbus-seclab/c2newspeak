Newspeak output
---------------
515.c
int32 f(int32 x) {
  (515.c:27#1070)^!return =(int32) x_int32;
}

void main(void) {
  (515.c:31#1103)^int32 y;
  (515.c:32#1108)^y =(int32) coerce[-2147483648,2147483647] (y_int32 + 1);
  (515.c:33#1121)^y <- f(2);
}


