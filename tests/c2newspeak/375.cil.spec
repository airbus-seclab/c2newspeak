Newspeak output
---------------
375.c
void main(void) {
  (375.c:29#1097)^int32 x;
  (375.c:29#1100)^int32 y;
  int32 tmp;
  (375.c:28#1073)^do {
    (375.c:31#1106)^while (1) {
      (375.c:32#1122)^tmp <- f();
      (375.c:32#1122)^choose {
       -->
        (375.c:32#1122)^guard(! (tmp_int32 ==_int32 0));
        (375.c:33#1144)^y =(int32) 1;
        (375.c:34#1157)^goto lbl0;
       -->
        (375.c:32#1122)^guard((tmp_int32 ==_int32 0));
        (375.c:32#1122)^choose {
         -->
          (375.c:32#1122)^guard(! (x_int32 ==_int32 0));
          (375.c:33#1144)^y =(int32) 1;
          (375.c:34#1157)^goto lbl0;
         -->
          (375.c:32#1122)^guard((x_int32 ==_int32 0));
        }
      }
    }
  } with lbl0: {
  }
}


