Warning: 584.c:31#0: goto statement accepted
Newspeak output
---------------
584.c
void main(void) {
  (584.c:27#6)^uint32 goto!lbl;
  (584.c:27#6)^goto!lbl =(uint32) 0;
  (584.c:27#6)^{
    int32 i;
    (584.c:28#2)^do {
      (584.c:28#2)^while (1) {
        (584.c:28#9)^choose {
         -->
          (584.c:28#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
          (584.c:28#9)^guard(! (i_int32 ==_int32 0));
          (584.c:29#7)^i =(int32) 1;
         -->
          (584.c:28#9)^choose {
           -->
            (584.c:28#9)^guard(! (goto!lbl_uint32 ==_uint32 0));
            (584.c:28#9)^guard((i_int32 ==_int32 0));
           -->
            (584.c:28#9)^guard((goto!lbl_uint32 ==_uint32 0));
          }
        }
        (584.c:28#2)^goto!lbl =(uint32) 1;
        (584.c:28#2)^choose {
         -->
          (584.c:28#2)^guard(goto!lbl_int32);
         -->
          (584.c:28#2)^guard(! goto!lbl_int32);
          (584.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


