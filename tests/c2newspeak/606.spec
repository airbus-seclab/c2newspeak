Warning: 606.c:35#0: goto statement accepted
Newspeak output
---------------
void (606.c:26#5)^main(void) {
  (606.c:27#6)^uint32 goto!lbl;
  (606.c:27#6)^goto!lbl =(uint32) 0;
  (606.c:27#6)^{
    int32 i;
    (606.c:28#2)^do {
      (606.c:28#2)^while (1) {
        (606.c:28#2)^while (1) {
          (606.c:28#2)^choose {
           -->
            (606.c:28#2)^guard(! (goto!lbl_uint32 ==_uint32 0));
           -->
            (606.c:28#2)^guard((goto!lbl_uint32 ==_uint32 0));
          }
          (606.c:29#4)^choose {
           -->
            (606.c:29#4)^guard(! goto!lbl_int32);
            (606.c:29#4)^i =(int32) 1;
           -->
            (606.c:29#4)^guard(goto!lbl_int32);
          }
          (606.c:28#2)^do {
            (606.c:30#4)^while (1) {
              (606.c:30#4)^choose {
               -->
                (606.c:30#4)^choose {
                 -->
                  (606.c:30#4)^guard(! (goto!lbl_uint32 ==_uint32 0));
                 -->
                  (606.c:30#4)^guard((goto!lbl_uint32 ==_uint32 0));
                  (606.c:30#4)^guard(! (i_int32 ==_int32 0));
                }
               -->
                (606.c:30#4)^guard((goto!lbl_uint32 ==_uint32 0));
                (606.c:30#4)^guard((i_int32 ==_int32 0));
                (606.c:30#4)^goto lbl5;
              }
              (606.c:30#14)^choose {
               -->
                (606.c:30#14)^guard(! goto!lbl_int32);
                (606.c:31#6)^i =(int32) 2;
               -->
                (606.c:30#14)^guard(goto!lbl_int32);
              }
              (606.c:32#4)^goto!lbl =(uint32) 0;
              (606.c:32#9)^i =(int32) 0;
            }
          } with lbl5:
        }
        (606.c:28#2)^goto!lbl =(uint32) 1;
        (606.c:28#2)^choose {
         -->
          (606.c:28#2)^guard(goto!lbl_int32);
         -->
          (606.c:28#2)^guard(! goto!lbl_int32);
          (606.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


