Warning: 607.c:35#0: goto statement accepted
Newspeak output
---------------
void (607.c:26#5)^main(void) {
  (607.c:27#6)^uint32 goto!lbl;
  (607.c:27#6)^goto!lbl =(uint32) 0;
  (607.c:27#6)^{
    int32 i;
    (607.c:28#2)^do {
      (607.c:28#2)^while (1) {
        (607.c:28#2)^while (1) {
          (607.c:28#2)^choose {
           -->
            (607.c:28#2)^guard(! (goto!lbl_uint32 ==_uint32 0));
           -->
            (607.c:28#2)^guard((goto!lbl_uint32 ==_uint32 0));
          }
          (607.c:29#4)^choose {
           -->
            (607.c:29#4)^guard(! goto!lbl_int32);
            (607.c:29#4)^i =(int32) 1;
           -->
            (607.c:29#4)^guard(goto!lbl_int32);
          }
          (607.c:30#11)^choose {
           -->
            (607.c:30#11)^guard(! (goto!lbl_uint32 ==_uint32 0));
            (607.c:30#11)^guard(! (i_int32 ==_int32 0));
            (607.c:30#11)^choose {
             -->
              (607.c:30#11)^guard(! goto!lbl_int32);
              (607.c:31#6)^i =(int32) 2;
             -->
              (607.c:30#11)^guard(goto!lbl_int32);
            }
            (607.c:32#9)^i =(int32) 0;
           -->
            (607.c:30#11)^choose {
             -->
              (607.c:30#11)^guard(! (goto!lbl_uint32 ==_uint32 0));
              (607.c:30#11)^guard((i_int32 ==_int32 0));
             -->
              (607.c:30#11)^guard((goto!lbl_uint32 ==_uint32 0));
            }
          }
        }
        (607.c:28#2)^goto!lbl =(uint32) 1;
        (607.c:28#2)^choose {
         -->
          (607.c:28#2)^guard(goto!lbl_int32);
         -->
          (607.c:28#2)^guard(! goto!lbl_int32);
          (607.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


