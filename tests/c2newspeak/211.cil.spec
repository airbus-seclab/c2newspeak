Newspeak output
---------------
211.c
int32 f(int32 z) {
  (211.c:29#1077)^!return =(int32) x_int32;
}

void main(void) {
  (211.c:33#1110)^int32 y;
  int32 tmp;
  (211.c:35#1118)^tmp =(int32) x_int32;
  (211.c:35#1118)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (211.c:35#1118)^y <- f(tmp_int32);
}

int32 x;

