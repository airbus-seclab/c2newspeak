Warning: 580.c:30#0: goto statement accepted
Newspeak output
---------------
580.c
void main(void) {
  (580.c:27#6)^uint32 goto.lbl;
  (580.c:27#6)^goto.lbl =(uint32) 0;
  (580.c:27#6)^{
    int32 i;
    (580.c:28#1)^do {
      (580.c:28#1)^while (1) {
        (580.c:29#2)^i =(int32) 1;
        (580.c:29#2)^do {
          (580.c:29#2)^while (1) {
            (580.c:29#2)^choose {
             -->
              (580.c:29#2)^guard(! (i_int32 ==_int32 0));
             -->
              (580.c:29#2)^guard((i_int32 ==_int32 0));
              (580.c:29#2)^goto lbl4;
            }
            (580.c:30#2)^goto.lbl =(uint32) 1;
            (580.c:30#2)^choose {
             -->
              (580.c:30#2)^guard(goto.lbl_int32);
              (580.c:30#2)^goto lbl4;
             -->
              (580.c:30#2)^guard(! goto.lbl_int32);
            }
          }
        } with lbl4: {
        }
        (580.c:28#1)^choose {
         -->
          (580.c:28#1)^guard(goto.lbl_int32);
         -->
          (580.c:28#1)^guard(! goto.lbl_int32);
          (580.c:28#1)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


