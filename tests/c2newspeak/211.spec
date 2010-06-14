Newspeak output
---------------
211.c
int32 f(int32 z) {
  (211.c:29#2)^!return =(int32) x_int32;
}

void main(void) {
  (211.c:33#6)^int32 y;
  (211.c:35#2)^int32 !tmp0;
  (211.c:35#2)^!tmp0 =(int32) x_int32;
  (211.c:35#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (211.c:35#2)^y <- f(!tmp0_int32);
}

int32 x;

