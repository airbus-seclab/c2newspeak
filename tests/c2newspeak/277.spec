Newspeak output
---------------
277.c
void main(void) {
  (277.c:28#6)^int32 x;
  (277.c:32#4)^int32 !tmp0;
  (277.c:30#2)^do {
    (277.c:31#2)^do {
      (277.c:30#2)^choose {
       -->
        (277.c:30#2)^guard((x_int32 ==_int32 0));
        (277.c:31#2)^goto lbl2;
       -->
        (277.c:30#2)^guard(! (x_int32 ==_int32 0));
        (277.c:30#2)^goto lbl1;
      }
    } with lbl2: {
    }
    (277.c:32#4)^!tmp0 <- g();
    (277.c:32#4)^choose {
     -->
      (277.c:32#4)^guard(! (!tmp0_int32 ==_int32 0));
     -->
      (277.c:32#4)^guard((!tmp0_int32 ==_int32 0));
    }
  } with lbl1: {
  }
}


