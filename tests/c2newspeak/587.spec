Warning: 587.c:36#1154: goto statement accepted
Newspeak output
---------------
587.c
void main(void) {
  (587.c:27#6)^uint32 goto.lbl;
  (587.c:27#6)^0- =(uint32) 0;
  (587.c:27#6)^{
    int32 i;
    (587.c:28#2)^1- =(uint32) 1-_uint32;
    (587.c:28#2)^{
      uint32 switch.587.c:28#2;
      (587.c:28#2)^choose {
       -->
        (587.c:28#2)^guard(! 2-_uint32);
        (587.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
       -->
        (587.c:28#2)^guard(2-_uint32);
        (587.c:28#2)^0- =(uint32) coerce[0,4294967295] ! 1;
      }
      (587.c:28#2)^do {
        (587.c:31#2)^do {
          (587.c:29#2)^do {
            (587.c:28#2)^choose {
             -->
              (587.c:28#2)^guard((0-_uint32 ==_int32 1));
              (587.c:29#2)^goto lbl6;
             -->
              (587.c:28#2)^choose {
               -->
                (587.c:28#2)^guard((0-_uint32 ==_int32 2));
                (587.c:31#2)^goto lbl5;
               -->
                (587.c:28#2)^guard(! (0-_uint32 ==_int32 2));
                (587.c:28#2)^guard(! (0-_uint32 ==_int32 1));
                (587.c:28#2)^goto lbl4;
              }
            }
          } with lbl6: {
          }
          (587.c:30#4)^1- =(int32) 2;
        } with lbl5: {
        }
        (587.c:32#4)^1- =(int32) 1;
      } with lbl4: {
      }
      (587.c:34#7)^1- =(int32) 0;
      (587.c:36#2)^2- =(uint32) 1;
    }
    (587.c:36#2)^do {
      (587.c:36#2)^while (1) {
        (587.c:36#2)^choose {
         -->
          (587.c:36#2)^guard(1-_uint32);
         -->
          (587.c:36#2)^guard(! 1-_uint32);
          (587.c:36#2)^goto lbl1;
        }
        (587.c:28#2)^1- =(uint32) 1-_uint32;
        (587.c:28#2)^{
          uint32 switch.587.c:28#2;
          (587.c:28#2)^choose {
           -->
            (587.c:28#2)^guard(! 2-_uint32);
            (587.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
           -->
            (587.c:28#2)^guard(2-_uint32);
            (587.c:28#2)^0- =(uint32) coerce[0,4294967295] ! 1;
          }
          (587.c:28#2)^do {
            (587.c:31#2)^do {
              (587.c:29#2)^do {
                (587.c:28#2)^choose {
                 -->
                  (587.c:28#2)^guard((0-_uint32 ==_int32 1));
                  (587.c:29#2)^goto lbl12;
                 -->
                  (587.c:28#2)^choose {
                   -->
                    (587.c:28#2)^guard((0-_uint32 ==_int32 2));
                    (587.c:31#2)^goto lbl11;
                   -->
                    (587.c:28#2)^guard(! (0-_uint32 ==_int32 2));
                    (587.c:28#2)^guard(! (0-_uint32 ==_int32 1));
                    (587.c:28#2)^goto lbl10;
                  }
                }
              } with lbl12: {
              }
              (587.c:30#4)^1- =(int32) 2;
            } with lbl11: {
            }
            (587.c:32#4)^1- =(int32) 1;
          } with lbl10: {
          }
          (587.c:34#7)^1- =(int32) 0;
          (587.c:36#2)^2- =(uint32) 1;
        }
      }
    } with lbl1: {
    }
  }
}


