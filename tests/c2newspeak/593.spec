Warning: 593.c:39#1172: goto statement accepted
Newspeak output
---------------
593.c
void main(void) {
  (593.c:27#6)^uint32 goto.lbl;
  (593.c:27#6)^0- =(uint32) 0;
  (593.c:27#6)^{
    int32 i;
    (593.c:29#2)^0- =(int32) 1;
    (593.c:28#1)^do {
      (593.c:29#2)^do {
        (593.c:34#2)^do {
          (593.c:30#2)^do {
            (593.c:29#2)^choose {
             -->
              (593.c:29#2)^guard((0-_int32 ==_int32 1));
              (593.c:30#2)^goto lbl6;
             -->
              (593.c:29#2)^choose {
               -->
                (593.c:29#2)^guard((0-_int32 ==_int32 2));
                (593.c:34#2)^goto lbl5;
               -->
                (593.c:29#2)^guard(! (0-_int32 ==_int32 2));
                (593.c:29#2)^guard(! (0-_int32 ==_int32 1));
                (593.c:29#2)^goto lbl4;
              }
            }
          } with lbl6: {
          }
          (593.c:31#4)^0- =(int32) 2;
          (593.c:32#4)^goto lbl2;
        } with lbl5: {
        }
        (593.c:35#4)^0- =(int32) 1;
        (593.c:36#4)^goto lbl2;
      } with lbl4: {
      }
      (593.c:39#4)^1- =(uint32) 1;
      (593.c:39#4)^choose {
       -->
        (593.c:39#4)^guard(1-_uint32);
        (593.c:39#4)^goto lbl2;
       -->
        (593.c:39#4)^guard(! 1-_uint32);
      }
    } with lbl2: {
    }
    (593.c:28#1)^do {
      (593.c:28#1)^while (1) {
        (593.c:28#1)^choose {
         -->
          (593.c:28#1)^guard(1-_uint32);
         -->
          (593.c:28#1)^guard(! 1-_uint32);
          (593.c:28#1)^goto lbl1;
        }
        (593.c:29#2)^0- =(int32) 1;
        (593.c:28#1)^do {
          (593.c:29#2)^do {
            (593.c:34#2)^do {
              (593.c:30#2)^do {
                (593.c:29#2)^choose {
                 -->
                  (593.c:29#2)^guard((0-_int32 ==_int32 1));
                  (593.c:30#2)^goto lbl11;
                 -->
                  (593.c:29#2)^choose {
                   -->
                    (593.c:29#2)^guard((0-_int32 ==_int32 2));
                    (593.c:34#2)^goto lbl10;
                   -->
                    (593.c:29#2)^guard(! (0-_int32 ==_int32 2));
                    (593.c:29#2)^guard(! (0-_int32 ==_int32 1));
                    (593.c:29#2)^goto lbl9;
                  }
                }
              } with lbl11: {
              }
              (593.c:31#4)^0- =(int32) 2;
              (593.c:32#4)^goto lbl7;
            } with lbl10: {
            }
            (593.c:35#4)^0- =(int32) 1;
            (593.c:36#4)^goto lbl7;
          } with lbl9: {
          }
          (593.c:39#4)^1- =(uint32) 1;
          (593.c:39#4)^choose {
           -->
            (593.c:39#4)^guard(1-_uint32);
            (593.c:39#4)^goto lbl7;
           -->
            (593.c:39#4)^guard(! 1-_uint32);
          }
        } with lbl7: {
        }
      }
    } with lbl1: {
    }
  }
}


