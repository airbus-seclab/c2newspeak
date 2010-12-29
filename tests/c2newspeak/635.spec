Warning: 635.c:39#0: goto statement accepted
Newspeak output
---------------
void (635.c:25#5)^main(void) {
  (635.c:27#2)^uint32 goto!lbl;
  (635.c:27#2)^goto!lbl =(uint32) 0;
  (635.c:29#10)^{
    int32 m;
    (635.c:37#2)^do {
      (635.c:37#2)^while (1) {
        (635.c:27#2)^while (1) {
          (635.c:27#2)^choose {
           -->
            (635.c:27#2)^guard(! (goto!lbl_uint32 ==_uint32 0));
           -->
            (635.c:27#2)^guard((goto!lbl_uint32 ==_uint32 0));
          }
          (635.c:33#10)^choose {
           -->
            (635.c:33#10)^guard((goto!lbl_uint32 ==_uint32 0));
            (635.c:31#1)^m =(int32) 0;
           -->
            (635.c:33#10)^guard(! (goto!lbl_uint32 ==_uint32 0));
          }
        }
        (635.c:37#2)^do {
          (635.c:37#2)^while (1) {
            (635.c:39#4)^goto!lbl =(uint32) 1;
            (635.c:39#4)^choose {
             -->
              (635.c:39#4)^guard(goto!lbl_int32);
              (635.c:39#4)^goto lbl7;
             -->
              (635.c:39#4)^guard(! goto!lbl_int32);
            }
          }
        } with lbl7:
        (635.c:37#2)^choose {
         -->
          (635.c:37#2)^guard(goto!lbl_int32);
         -->
          (635.c:37#2)^guard(! goto!lbl_int32);
          (635.c:37#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


