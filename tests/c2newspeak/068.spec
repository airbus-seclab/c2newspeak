Newspeak output
---------------
068.c
int32 f(void) {
  (068.c:27#2)^!return =(int32) 1;
}

void main(void) {
  (068.c:31#6)^int32 x;
  (068.c:32#2)^int32 !tmp0;
  (068.c:32#2)^do {
    (068.c:32#2)^while (1) {
      (068.c:32#2)^!tmp0 <- f();
      (068.c:32#2)^choose {
       -->
        (068.c:32#2)^guard(! (!tmp0_int32 ==_int32 0));
       -->
        (068.c:32#2)^guard((!tmp0_int32 ==_int32 0));
        (068.c:32#2)^goto lbl1;
      }
      (068.c:33#4)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    }
  } with lbl1: {
  }
}


