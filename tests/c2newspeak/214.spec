Warning: 214.c:32#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
214.c
void f(int32 x, int32 y) {
}

void main(void) {
  (214.c:31#6)^int32 x;
  (214.c:31#6)^x =(int32) 0;
  (214.c:32#2)^{
    int32 !tmp0;
    (214.c:32#2)^!tmp0 =(int32) x_int32;
    (214.c:32#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    (214.c:32#2)^{
      int32 !tmp1;
      (214.c:32#2)^!tmp1 =(int32) x_int32;
      (214.c:32#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
      (214.c:32#2)^f(!tmp1_int32, !tmp0_int32);
    }
  }
}


