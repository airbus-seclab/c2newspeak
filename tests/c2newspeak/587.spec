Warning: 587.c:39#1187: goto statement accepted
Newspeak output
---------------
587.c
void main(void) {
  (587.c:27#6)^uint32 goto.lbl;
  (587.c:27#6)^0- =(uint32) 0;
  (587.c:27#6)^{
    int32 i;
    (587.c:28#2)^uint32 switch.587.c:28#2.0;
    (587.c:28#2)^do {
      (587.c:28#2)^while (1) {
        (587.c:28#2)^choose {
         -->
          (587.c:28#2)^guard(! 2-_int32);
          (587.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
         -->
          (587.c:28#2)^guard(2-_int32);
          (587.c:28#2)^0- =(uint32) 0;
        }
        (587.c:28#2)^do {
          (587.c:28#2)^do {
            (587.c:32#2)^do {
              (587.c:29#2)^do {
                (587.c:28#2)^choose {
                 -->
                  (587.c:28#2)^guard((0-_int32 ==_int32 1));
                  (587.c:29#2)^goto lbl6;
                 -->
                  (587.c:28#2)^choose {
                   -->
                    (587.c:28#2)^guard((0-_int32 ==_int32 2));
                    (587.c:32#2)^goto lbl5;
                   -->
                    (587.c:28#2)^guard(! (0-_int32 ==_int32 2));
                    (587.c:28#2)^guard(! (0-_int32 ==_int32 1));
                    (587.c:28#2)^goto lbl4;
                  }
                }
              } with lbl6: {
              }
              (587.c:30#4)^1- =(int32) 2;
              (587.c:31#4)^goto lbl3;
            } with lbl5: {
            }
            (587.c:33#4)^1- =(int32) 1;
            (587.c:34#4)^goto lbl3;
          } with lbl4: {
          }
          (587.c:36#7)^1- =(int32) 0;
        } with lbl3: {
        }
        (587.c:28#2)^2- =(uint32) 1;
        (587.c:28#2)^choose {
         -->
          (587.c:28#2)^guard(2-_int32);
         -->
          (587.c:28#2)^guard(! 2-_int32);
          (587.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


