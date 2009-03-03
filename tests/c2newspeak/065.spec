Warning: 065.c:32#2: the order of execution of side-effects in expressions not specified, picking a random one, be careful
Newspeak output
---------------
065.c
int32 f(int32) {
  (065.c:27#2)^1- =(int32) 0-_int32;
}

void main(void) {
  (065.c:31#6)^int32 x;
  (065.c:32#2)^int32 !tmp0;
  (065.c:32#2)^{
    int32 f.arg1;
    (065.c:32#2)^0- =(int32) 2;
    (065.c:32#2)^f();
  }
  (065.c:32#2)^{
    int32 !tmp1;
    (065.c:32#2)^{
      int32 f.arg1;
      (065.c:32#2)^0- =(int32) 3;
      (065.c:32#2)^f();
    }
    (065.c:32#2)^2- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 0-_int32);
  }
}


