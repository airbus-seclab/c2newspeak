Newspeak output
---------------
734.c
void main(void) {
  (734.c:29#1066)^int32 a;
  (734.c:30#1075)^int32 b;
  (734.c:28#1046)^do {
    (734.c:32#1081)^while (1) {
      (734.c:32#1081)^choose {
       -->
        (734.c:32#1081)^guard(! (a_int32 ==_int32 0));
        (734.c:32#1081)^choose {
         -->
          (734.c:32#1081)^guard(! (b_int32 ==_int32 0));
         -->
          (734.c:32#1081)^guard((b_int32 ==_int32 0));
          (734.c:32#1081)^goto lbl0;
        }
       -->
        (734.c:32#1081)^guard((a_int32 ==_int32 0));
        (734.c:32#1081)^goto lbl0;
      }
    }
  } with lbl0: {
  }
}


