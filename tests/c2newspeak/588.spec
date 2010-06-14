Warning: 588.c:30#0: goto statement accepted
Newspeak output
---------------
588.c
void main(void) {
  (588.c:27#6)^uint32 goto.lbl;
  (588.c:27#6)^goto.lbl =(uint32) 0;
  (588.c:27#6)^{
    int32 i;
    (588.c:28#1)^do {
      (588.c:28#1)^while (1) {
        (588.c:29#2)^i =(int32) 1;
        (588.c:29#2)^choose {
         -->
          (588.c:29#2)^guard(! (i_int32 ==_int32 0));
          (588.c:29#2)^goto.lbl =(uint32) 1;
         -->
          (588.c:29#2)^guard((i_int32 ==_int32 0));
        }
        (588.c:28#1)^choose {
         -->
          (588.c:28#1)^guard(goto.lbl_int32);
         -->
          (588.c:28#1)^guard(! goto.lbl_int32);
          (588.c:28#1)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


