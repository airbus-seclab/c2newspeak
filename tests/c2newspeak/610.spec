Warning: 610.c:34#6: goto statement accepted
Newspeak output
---------------
void (610.c:26#5)^main(void) {
  (610.c:27#6)^uint32 goto!lbl;
  (610.c:27#6)^goto!lbl =(uint32) 0;
  (610.c:27#6)^{
    int32 i;
    (610.c:28#2)^do {
      (610.c:28#2)^do {
        (610.c:29#2)^do {
          (610.c:28#2)^choose {
           -->
            (610.c:28#2)^guard((i_int32 ==_int32 0));
            (610.c:29#2)^goto lbl3;
           -->
            (610.c:28#2)^guard(! (i_int32 ==_int32 0));
            (610.c:28#2)^goto lbl2;
          }
        } with lbl3:
        (610.c:30#4)^goto lbl1;
      } with lbl2:
      (610.c:32#4)^do {
        (610.c:33#4)^do {
          (610.c:32#4)^choose {
           -->
            (610.c:32#4)^guard((i_int32 ==_int32 1));
            (610.c:33#4)^goto lbl5;
           -->
            (610.c:32#4)^guard(! (i_int32 ==_int32 1));
            (610.c:32#4)^goto lbl4;
          }
        } with lbl5:
        (610.c:34#6)^goto!lbl =(uint32) 1;
        (610.c:34#6)^choose {
         -->
          (610.c:34#6)^guard(goto!lbl_int32);
          (610.c:34#6)^goto lbl4;
         -->
          (610.c:34#6)^guard(! goto!lbl_int32);
        }
        (610.c:35#6)^i =(int32) 2;
      } with lbl4:
      (610.c:34#6)^choose {
       -->
        (610.c:34#6)^guard(goto!lbl_int32);
        (610.c:34#6)^goto lbl1;
       -->
        (610.c:34#6)^guard(! goto!lbl_int32);
      }
      (610.c:38#4)^i =(int32) 1;
    } with lbl1:
    (610.c:41#2)^while (1) {
      (610.c:41#2)^choose {
       -->
        (610.c:41#2)^guard(! (goto!lbl_uint32 ==_uint32 0));
       -->
        (610.c:41#2)^guard((goto!lbl_uint32 ==_uint32 0));
      }
      (610.c:42#2)^goto!lbl =(uint32) 0;
    }
  }
}


