Newspeak output
---------------
219.c
int32 f(void) {
  (219.c:27#2)^!return =(int32) 0;
}

void main(void) {
  (219.c:31#6)^int32 x;
  (219.c:32#2)^{
    int32 !tmp0;
    (219.c:32#2)^!tmp0 <- f();
    (219.c:32#2)^x =(int32) coerce[-2147483648,2147483647] (!tmp0_int32 + 1);
  }
  (219.c:33#2)^x =(int32) 2;
}


