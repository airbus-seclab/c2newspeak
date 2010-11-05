Warning: 628.c:32#0: goto statement accepted
Warning: 628.c:33#0: goto statement accepted
Newspeak output
---------------
628.c
void main(void) {
  (628.c:27#2)^uint32 goto!lbl;
  (628.c:27#2)^goto!lbl =(uint32) 0;
  (628.c:27#2)^while (1) {
  }
  (628.c:33#2)^do {
    (628.c:33#2)^while (1) {
      (628.c:29#9)^do {
        (628.c:29#9)^while (1) {
          (628.c:29#9)^choose {
           -->
            (628.c:29#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
            (628.c:29#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
           -->
            (628.c:29#9)^choose {
             -->
              (628.c:29#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
              (628.c:29#9)^guard((goto!lbl_uint32 ==_uint32 0));
             -->
              (628.c:29#9)^guard((goto!lbl_uint32 ==_uint32 0));
            }
          }
          (628.c:29#9)^goto!lbl =(uint32) 1;
          (628.c:29#9)^choose {
           -->
            (628.c:29#9)^guard(goto!lbl_int32);
           -->
            (628.c:29#9)^guard(! goto!lbl_int32);
            (628.c:29#9)^goto lbl6;
          }
        }
      } with lbl6:
      (628.c:33#17)^goto!lbl =(uint32) 1;
      (628.c:33#2)^choose {
       -->
        (628.c:33#2)^guard(goto!lbl_int32);
       -->
        (628.c:33#2)^guard(! goto!lbl_int32);
        (628.c:33#2)^goto lbl4;
      }
    }
  } with lbl4:
}


