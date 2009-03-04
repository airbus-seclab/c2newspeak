Warning: 621.c:30#1103: goto statement accepted
Warning: 621.c:31#1119: goto statement accepted
Newspeak output
---------------
621.c
void main(void) {
  (621.c:27#4)^uint32 break.621.c:29#13.0;
  (621.c:27#4)^0- =(uint32) 0;
  (621.c:27#4)^{
    uint32 break.621.c:29#13.1;
    (621.c:27#4)^0- =(uint32) 0;
    (621.c:27#4)^{
      uint32 goto.lbl;
      (621.c:27#4)^0- =(uint32) 0;
      (621.c:27#4)^do {
        (621.c:27#4)^while (1) {
          (621.c:27#13)^do {
            (621.c:27#13)^do {
              (621.c:27#13)^1- =(uint32) 1;
              (621.c:29#13)^2- =(uint32) 1;
              (621.c:29#13)^goto lbl6;
              (621.c:27#13)^while (1) {
                (621.c:27#13)^1- =(uint32) 1;
                (621.c:29#13)^2- =(uint32) 1;
                (621.c:29#13)^goto lbl6;
              }
            } with lbl6: {
            }
            (621.c:27#13)^choose {
             -->
              (621.c:27#13)^guard(2-_uint32);
              (621.c:27#13)^2- =(uint32) 0;
              (621.c:27#13)^goto lbl4;
             -->
              (621.c:27#13)^guard(! 2-_uint32);
            }
            (621.c:27#13)^while (1) {
              (621.c:27#13)^do {
                (621.c:27#13)^1- =(uint32) 1;
                (621.c:29#13)^2- =(uint32) 1;
                (621.c:29#13)^goto lbl12;
                (621.c:27#13)^while (1) {
                  (621.c:27#13)^1- =(uint32) 1;
                  (621.c:29#13)^2- =(uint32) 1;
                  (621.c:29#13)^goto lbl12;
                }
              } with lbl12: {
              }
              (621.c:27#13)^choose {
               -->
                (621.c:27#13)^guard(2-_uint32);
                (621.c:27#13)^2- =(uint32) 0;
                (621.c:27#13)^goto lbl4;
               -->
                (621.c:27#13)^guard(! 2-_uint32);
              }
            }
          } with lbl4: {
          }
          (621.c:27#13)^choose {
           -->
            (621.c:27#13)^guard(1-_uint32);
            (621.c:27#13)^1- =(uint32) 0;
            (621.c:27#13)^goto lbl1;
           -->
            (621.c:27#13)^guard(! 1-_uint32);
          }
        }
      } with lbl1: {
      }
    }
  }
}


