Warning: 598.c:37#1162: goto statement accepted
Newspeak output
---------------
598.c
void main(void) {
  (598.c:27#6)^uint32 goto.lbl;
  (598.c:27#6)^0- =(uint32) 0;
  (598.c:27#6)^{
    int32 i;
    (598.c:27#9)^int32 j;
    (598.c:33#2)^do {
      (598.c:33#2)^while (1) {
        (598.c:28#2)^do {
          (598.c:28#2)^while (1) {
            (598.c:28#2)^choose {
             -->
              (598.c:28#2)^choose {
               -->
                (598.c:28#2)^guard(2-_uint32);
               -->
                (598.c:28#2)^guard(! 2-_uint32);
                (598.c:28#2)^guard(! (1-_int32 ==_int32 0));
              }
             -->
              (598.c:28#2)^guard(! 2-_uint32);
              (598.c:28#2)^guard((1-_int32 ==_int32 0));
              (598.c:28#2)^goto lbl3;
            }
            (598.c:28#2)^do {
              (598.c:29#4)^while (1) {
                (598.c:29#4)^choose {
                 -->
                  (598.c:29#4)^choose {
                   -->
                    (598.c:29#4)^guard(2-_uint32);
                   -->
                    (598.c:29#4)^guard(! 2-_uint32);
                    (598.c:29#4)^guard(! (0-_int32 ==_int32 0));
                  }
                 -->
                  (598.c:29#4)^guard(! 2-_uint32);
                  (598.c:29#4)^guard((0-_int32 ==_int32 0));
                  (598.c:29#4)^goto lbl5;
                }
                (598.c:30#4)^2- =(uint32) 0;
                (598.c:30#9)^1- =(int32) 0;
              }
            } with lbl5: {
            }
          }
        } with lbl3: {
        }
        (598.c:33#2)^choose {
         -->
          (598.c:33#2)^guard(! (0-_int32 ==_int32 0));
         -->
          (598.c:33#2)^guard((0-_int32 ==_int32 0));
          (598.c:36#4)^choose {
           -->
            (598.c:36#4)^guard(! (1-_int32 ==_int32 0));
            (598.c:33#2)^2- =(uint32) 1;
           -->
            (598.c:36#4)^guard((1-_int32 ==_int32 0));
          }
        }
        (598.c:33#2)^choose {
         -->
          (598.c:33#2)^guard(2-_uint32);
         -->
          (598.c:33#2)^guard(! 2-_uint32);
          (598.c:33#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


