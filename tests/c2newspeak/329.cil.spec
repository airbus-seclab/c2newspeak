Newspeak output
---------------
329.c
void main(void) {
  int32 tmp;
  (329.c:29#1089)^f();
  (329.c:28#1073)^do {
    (329.c:29#1089)^choose {
     -->
      (329.c:29#1089)^guard(! (0-_int32 ==_int32 1));
      (329.c:29#1089)^goto lbl0;
     -->
      (329.c:29#1089)^choose {
       -->
        (329.c:29#1089)^guard((0-_int32 ==_int32 1));
        (329.c:29#1089)^goto lbl0;
       -->
      }
    }
  } with lbl0: {
  }
}


