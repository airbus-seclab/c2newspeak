Newspeak output
---------------
int32 f(int32 x) {
  (515.c:27#2)^!return =(int32) x_int32;
}

void main(void) {
  (515.c:31#6)^int32 y;
  (515.c:32#2)^y =(int32) coerce[-2147483648,2147483647] (y_int32 + 1);
  (515.c:33#2)^y: int32 <- f(2: int32);
}


