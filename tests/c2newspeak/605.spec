Warning: 605.c:34#0: goto statement accepted
Newspeak output
---------------
605.c
void main(void) {
  (605.c:27#6)^uint32 goto.lbl;
  (605.c:27#6)^goto.lbl =(uint32) 0;
  (605.c:27#6)^{
    int32 i;
    (605.c:28#2)^do {
      (605.c:28#2)^while (1) {
        (605.c:28#2)^while (1) {
          (605.c:28#2)^choose {
           -->
            (605.c:28#2)^guard(! (goto.lbl_uint32 ==_uint32 0));
           -->
            (605.c:28#2)^guard((goto.lbl_uint32 ==_uint32 0));
          }
          (605.c:29#4)^choose {
           -->
            (605.c:29#4)^guard(! goto.lbl_int32);
            (605.c:29#4)^i =(int32) 1;
           -->
            (605.c:29#4)^guard(goto.lbl_int32);
          }
          (605.c:28#2)^do {
            (605.c:30#4)^while (1) {
              (605.c:30#4)^choose {
               -->
                (605.c:30#4)^choose {
                 -->
                  (605.c:30#4)^guard(! (goto.lbl_uint32 ==_uint32 0));
                 -->
                  (605.c:30#4)^guard((goto.lbl_uint32 ==_uint32 0));
                  (605.c:30#4)^guard(! (i_int32 ==_int32 0));
                }
               -->
                (605.c:30#4)^guard((goto.lbl_uint32 ==_uint32 0));
                (605.c:30#4)^guard((i_int32 ==_int32 0));
                (605.c:30#4)^goto lbl5;
              }
              (605.c:31#4)^goto.lbl =(uint32) 0;
              (605.c:31#9)^i =(int32) 0;
            }
          } with lbl5: {
          }
        }
        (605.c:28#2)^goto.lbl =(uint32) 1;
        (605.c:28#2)^choose {
         -->
          (605.c:28#2)^guard(goto.lbl_int32);
         -->
          (605.c:28#2)^guard(! goto.lbl_int32);
          (605.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


