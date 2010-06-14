Newspeak output
---------------
068.c
int32 f(void) {
  (068.c:27#1064)^!return =(int32) 1;
}

void main(void) {
  (068.c:31#1097)^int32 x;
  int32 tmp;
  (068.c:30#1077)^do {
    (068.c:32#1102)^while (1) {
      (068.c:32#1102)^tmp <- f();
      (068.c:32#1102)^choose {
       -->
        (068.c:32#1102)^guard(! (tmp_int32 ==_int32 0));
       -->
        (068.c:32#1102)^guard((tmp_int32 ==_int32 0));
        (068.c:32#1102)^goto lbl0;
      }
      (068.c:33#1120)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
    }
  } with lbl0: {
  }
}


