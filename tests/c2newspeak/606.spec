Warning: 606.c:35#1147: goto statement accepted
Newspeak output
---------------
606.c
void main(void) {
  (606.c:27#6)^uint32 goto.lbl;
  (606.c:27#6)^0- =(uint32) 0;
  (606.c:27#6)^{
    int32 i;
    (606.c:28#2)^do {
      (606.c:28#2)^while (1) {
        (606.c:28#2)^1- =(uint32) 1-_uint32;
        (606.c:28#2)^while (1) {
          (606.c:28#2)^choose {
           -->
            (606.c:28#2)^guard(1-_uint32);
           -->
            (606.c:28#2)^guard(! 1-_uint32);
          }
          (606.c:29#4)^1- =(uint32) 1-_uint32;
          (606.c:29#4)^choose {
           -->
            (606.c:29#4)^guard(! 1-_uint32);
            (606.c:29#4)^0- =(int32) 1;
           -->
            (606.c:29#4)^guard(1-_uint32);
          }
          (606.c:28#2)^do {
            (606.c:30#4)^while (1) {
              (606.c:30#4)^choose {
               -->
                (606.c:30#4)^guard(1-_uint32);
               -->
                (606.c:30#4)^guard(! 1-_uint32);
                (606.c:30#4)^choose {
                 -->
                  (606.c:30#4)^guard(! (0-_int32 ==_int32 0));
                 -->
                  (606.c:30#4)^guard((0-_int32 ==_int32 0));
                  (606.c:30#4)^goto lbl5;
                }
              }
              (606.c:30#14)^choose {
               -->
                (606.c:30#14)^guard(! 1-_uint32);
                (606.c:31#6)^0- =(int32) 2;
               -->
                (606.c:30#14)^guard(1-_uint32);
              }
              (606.c:32#4)^1- =(uint32) 0;
              (606.c:32#9)^0- =(int32) 0;
            }
          } with lbl5: {
          }
        }
        (606.c:28#2)^1- =(uint32) 1;
        (606.c:28#2)^choose {
         -->
          (606.c:28#2)^guard(1-_uint32);
         -->
          (606.c:28#2)^guard(! 1-_uint32);
          (606.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


