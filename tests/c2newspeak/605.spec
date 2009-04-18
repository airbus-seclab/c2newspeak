Warning: 605.c:34#1134: goto statement accepted
Newspeak output
---------------
605.c
void main(void) {
  (605.c:27#6)^uint32 goto.lbl;
  (605.c:27#6)^0- =(uint32) 0;
  (605.c:27#6)^{
    int32 i;
    (605.c:28#2)^do {
      (605.c:28#2)^while (1) {
        (605.c:28#2)^while (1) {
          (605.c:28#2)^choose {
           -->
            (605.c:28#2)^guard(1-_uint32);
           -->
            (605.c:28#2)^guard(! 1-_uint32);
          }
          (605.c:29#4)^choose {
           -->
            (605.c:29#4)^guard(! 1-_uint32);
            (605.c:29#4)^0- =(int32) 1;
           -->
            (605.c:29#4)^guard(1-_uint32);
          }
          (605.c:28#2)^do {
            (605.c:30#4)^while (1) {
              (605.c:30#4)^choose {
               -->
                (605.c:30#4)^choose {
                 -->
                  (605.c:30#4)^guard(1-_uint32);
                 -->
                  (605.c:30#4)^guard(! 1-_uint32);
                  (605.c:30#4)^guard(! (0-_int32 ==_int32 0));
                }
               -->
                (605.c:30#4)^guard(! 1-_uint32);
                (605.c:30#4)^guard((0-_int32 ==_int32 0));
                (605.c:30#4)^goto lbl5;
              }
              (605.c:31#4)^1- =(uint32) 0;
              (605.c:31#9)^0- =(int32) 0;
            }
          } with lbl5: {
          }
        }
        (605.c:28#2)^1- =(uint32) 1;
        (605.c:28#2)^choose {
         -->
          (605.c:28#2)^guard(1-_uint32);
         -->
          (605.c:28#2)^guard(! 1-_uint32);
          (605.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


