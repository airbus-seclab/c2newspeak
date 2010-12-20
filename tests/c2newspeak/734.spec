Newspeak output
---------------
void main(void) {
  (734.c:29#6)^int32 a;
  (734.c:30#6)^int32 b;
  (734.c:32#2)^do {
    (734.c:32#2)^while (1) {
      (734.c:32#2)^choose {
       -->
        (734.c:32#2)^guard(! (a_int32 ==_int32 0));
        (734.c:32#2)^guard(! (b_int32 ==_int32 0));
       -->
        (734.c:32#2)^choose {
         -->
          (734.c:32#2)^guard(! (a_int32 ==_int32 0));
          (734.c:32#2)^guard((b_int32 ==_int32 0));
         -->
          (734.c:32#2)^guard((a_int32 ==_int32 0));
        }
        (734.c:32#2)^goto lbl1;
      }
    }
  } with lbl1:
}


