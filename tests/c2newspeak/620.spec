Warning: 620.c:32#2: goto statement accepted
Warning: 620.c:34#2: goto statement accepted
Newspeak output
---------------
void (620.c:26#5)^main(void) {
  (620.c:27#6)^uint32 goto!lbl;
  (620.c:27#6)^goto!lbl =(uint32) 0;
  (620.c:27#6)^{
    int32 i;
    (620.c:28#2)^uint32 switch.620.c:28#2.1;
    (620.c:28#2)^do {
      (620.c:28#2)^while (1) {
        (620.c:28#2)^choose {
         -->
          (620.c:28#2)^guard(! goto!lbl_int32);
          (620.c:28#2)^switch.620.c:28#2.1 =(uint32) coerce[0,4294967295] i_int32;
         -->
          (620.c:28#2)^guard(goto!lbl_int32);
          (620.c:28#2)^switch.620.c:28#2.1 =(uint32) 0;
        }
        (620.c:28#2)^{
          uint32 switch.620.c:28#2.0;
          (620.c:28#2)^do {
            (620.c:28#2)^while (1) {
              (620.c:28#2)^choose {
               -->
                (620.c:28#2)^guard(! goto!lbl_int32);
                (620.c:28#2)^switch.620.c:28#2.0 =(uint32) switch.620.c:28#2.1_uint32;
               -->
                (620.c:28#2)^guard(goto!lbl_int32);
                (620.c:28#2)^switch.620.c:28#2.0 =(uint32) 0;
              }
              (620.c:33#2)^do {
                (620.c:33#2)^do {
                  (620.c:31#2)^do {
                    (620.c:29#2)^do {
                      (620.c:28#2)^choose {
                       -->
                        (620.c:28#2)^guard((switch.620.c:28#2.0_uint32 ==_int32 0));
                        (620.c:29#2)^goto lbl8;
                       -->
                        (620.c:28#2)^choose {
                         -->
                          (620.c:28#2)^guard((switch.620.c:28#2.0_uint32 ==_int32 1));
                          (620.c:31#2)^goto lbl7;
                         -->
                          (620.c:28#2)^choose {
                           -->
                            (620.c:28#2)^guard((switch.620.c:28#2.0_uint32 ==_int32 2));
                            (620.c:33#2)^goto lbl6;
                           -->
                            (620.c:28#2)^guard(! (switch.620.c:28#2.0_uint32 ==_int32 2));
                            (620.c:28#2)^guard(! (switch.620.c:28#2.0_uint32 ==_int32 1));
                            (620.c:28#2)^guard(! (switch.620.c:28#2.0_uint32 ==_int32 0));
                            (620.c:28#2)^goto lbl5;
                          }
                        }
                      }
                    } with lbl8:
                    (620.c:30#7)^goto lbl5;
                  } with lbl7:
                  (620.c:32#2)^goto!lbl =(uint32) 1;
                  (620.c:32#2)^choose {
                   -->
                    (620.c:32#2)^guard(goto!lbl_int32);
                    (620.c:32#2)^goto lbl5;
                   -->
                    (620.c:32#2)^guard(! goto!lbl_int32);
                  }
                } with lbl6:
                (620.c:34#2)^goto!lbl =(uint32) 1;
                (620.c:34#2)^choose {
                 -->
                  (620.c:34#2)^guard(goto!lbl_int32);
                  (620.c:34#2)^goto lbl5;
                 -->
                  (620.c:34#2)^guard(! goto!lbl_int32);
                }
              } with lbl5:
              (620.c:28#2)^choose {
               -->
                (620.c:28#2)^guard(goto!lbl_int32);
               -->
                (620.c:28#2)^guard(! goto!lbl_int32);
                (620.c:28#2)^goto lbl3;
              }
            }
          } with lbl3:
        }
        (620.c:28#2)^choose {
         -->
          (620.c:28#2)^guard(goto!lbl_int32);
         -->
          (620.c:28#2)^guard(! goto!lbl_int32);
          (620.c:28#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


