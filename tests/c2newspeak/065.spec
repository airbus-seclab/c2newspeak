Warning: 065.c:32#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
065.c
int32 f(int32 a) {
  (065.c:27#2)^!return =(int32) a_int32;
}

void main(void) {
  (065.c:31#6)^int32 x;
  (065.c:32#2)^int32 !tmp0;
  (065.c:32#2)^!tmp0 <- f(2);
  (065.c:32#2)^{
    int32 !tmp1;
    (065.c:32#2)^!tmp1 <- f(3);
    (065.c:32#2)^x =(int32) coerce[-2147483648,2147483647] (!tmp0_int32 + !tmp1_int32);
  }
}


