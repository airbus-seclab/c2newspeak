Newspeak output
---------------
375.c
void main(void) {
  (375.c:29#6)^int32 x;
  (375.c:29#9)^int32 y;
  (375.c:32#4)^int32 !tmp0;
  (375.c:31#2)^do {
    (375.c:31#2)^while (1) {
      (375.c:32#4)^f();
      (375.c:32#4)^choose {
       -->
        (375.c:32#4)^choose {
         -->
          (375.c:32#4)^guard(! (0-_int32 ==_int32 0));
         -->
          (375.c:32#4)^guard((0-_int32 ==_int32 0));
          (375.c:32#4)^guard(! (2-_int32 ==_int32 0));
        }
        (375.c:33#6)^1- =(int32) 1;
        (375.c:34#6)^goto lbl1;
       -->
        (375.c:32#4)^guard((0-_int32 ==_int32 0));
        (375.c:32#4)^guard((2-_int32 ==_int32 0));
      }
    }
  } with lbl1: {
  }
}


