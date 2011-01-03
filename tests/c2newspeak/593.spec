Warning: 593.c:39#0: goto statement accepted
Newspeak output
---------------
void (593.c:26#5)^main(void) {
  (593.c:27#6)^uint32 goto!lbl;
  (593.c:27#6)^goto!lbl =(uint32) 0;
  (593.c:27#6)^{
    int32 i;
    (593.c:28#1)^do {
      (593.c:28#1)^while (1) {
        (593.c:29#2)^i =(int32) 1;
        (593.c:29#2)^do {
          (593.c:29#2)^do {
            (593.c:34#2)^do {
              (593.c:30#2)^do {
                (593.c:29#2)^choose {
                 -->
                  (593.c:29#2)^guard((i_int32 ==_int32 1));
                  (593.c:30#2)^goto lbl7;
                 -->
                  (593.c:29#2)^choose {
                   -->
                    (593.c:29#2)^guard((i_int32 ==_int32 2));
                    (593.c:34#2)^goto lbl6;
                   -->
                    (593.c:29#2)^guard(! (i_int32 ==_int32 2));
                    (593.c:29#2)^guard(! (i_int32 ==_int32 1));
                    (593.c:29#2)^goto lbl5;
                  }
                }
              } with lbl7:
              (593.c:31#4)^i =(int32) 2;
              (593.c:32#4)^goto lbl4;
            } with lbl6:
            (593.c:35#4)^i =(int32) 1;
            (593.c:36#4)^goto lbl4;
          } with lbl5:
          (593.c:39#4)^goto!lbl =(uint32) 1;
          (593.c:39#4)^choose {
           -->
            (593.c:39#4)^guard(goto!lbl_int32);
            (593.c:39#4)^goto lbl4;
           -->
            (593.c:39#4)^guard(! goto!lbl_int32);
          }
        } with lbl4:
        (593.c:28#1)^choose {
         -->
          (593.c:28#1)^guard(goto!lbl_int32);
         -->
          (593.c:28#1)^guard(! goto!lbl_int32);
          (593.c:28#1)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


