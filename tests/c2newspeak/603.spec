Warning: 603.c:30#1100: goto statement accepted
Newspeak output
---------------
603.c
void main(void) {
  (603.c:27#6)^uint32 goto.lbl;
  (603.c:27#6)^0- =(uint32) 0;
  (603.c:27#6)^{
    int32 i;
    (603.c:28#2)^1- =(uint32) 1-_uint32;
    (603.c:28#2)^{
      uint32 switch.603.c:28#2;
      (603.c:28#2)^choose {
       -->
        (603.c:28#2)^guard(! 2-_uint32);
        (603.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
       -->
        (603.c:28#2)^guard(2-_uint32);
        (603.c:28#2)^0- =(uint32) coerce[0,4294967295] ! 0;
      }
      (603.c:28#2)^do {
        (603.c:28#2)^do {
          (603.c:29#2)^do {
            (603.c:28#2)^choose {
             -->
              (603.c:28#2)^guard((0-_uint32 ==_int32 0));
              (603.c:29#2)^goto lbl5;
             -->
              (603.c:28#2)^guard(! (0-_uint32 ==_int32 0));
              (603.c:28#2)^goto lbl4;
            }
          } with lbl5: {
          }
          (603.c:30#4)^2- =(uint32) 1;
          (603.c:30#4)^choose {
           -->
            (603.c:30#4)^guard(2-_uint32);
            (603.c:30#4)^goto lbl3;
           -->
            (603.c:30#4)^guard(! 2-_uint32);
          }
        } with lbl4: {
        }
        (603.c:32#4)^2- =(uint32) 2-_uint32;
        (603.c:33#4)^2- =(uint32) 0;
        (603.c:32#4)^while (1) {
          (603.c:33#4)^2- =(uint32) 0;
        }
      } with lbl3: {
      }
      (603.c:30#4)^2- =(uint32) 2-_uint32;
    }
    (603.c:30#4)^do {
      (603.c:30#4)^while (1) {
        (603.c:30#4)^choose {
         -->
          (603.c:30#4)^guard(1-_uint32);
         -->
          (603.c:30#4)^guard(! 1-_uint32);
          (603.c:30#4)^goto lbl1;
        }
        (603.c:28#2)^1- =(uint32) 1-_uint32;
        (603.c:28#2)^{
          uint32 switch.603.c:28#2;
          (603.c:28#2)^choose {
           -->
            (603.c:28#2)^guard(! 2-_uint32);
            (603.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
           -->
            (603.c:28#2)^guard(2-_uint32);
            (603.c:28#2)^0- =(uint32) coerce[0,4294967295] ! 0;
          }
          (603.c:28#2)^do {
            (603.c:28#2)^do {
              (603.c:29#2)^do {
                (603.c:28#2)^choose {
                 -->
                  (603.c:28#2)^guard((0-_uint32 ==_int32 0));
                  (603.c:29#2)^goto lbl14;
                 -->
                  (603.c:28#2)^guard(! (0-_uint32 ==_int32 0));
                  (603.c:28#2)^goto lbl13;
                }
              } with lbl14: {
              }
              (603.c:30#4)^2- =(uint32) 1;
              (603.c:30#4)^choose {
               -->
                (603.c:30#4)^guard(2-_uint32);
                (603.c:30#4)^goto lbl12;
               -->
                (603.c:30#4)^guard(! 2-_uint32);
              }
            } with lbl13: {
            }
            (603.c:32#4)^2- =(uint32) 2-_uint32;
            (603.c:33#4)^2- =(uint32) 0;
            (603.c:32#4)^while (1) {
              (603.c:33#4)^2- =(uint32) 0;
            }
          } with lbl12: {
          }
          (603.c:30#4)^2- =(uint32) 2-_uint32;
        }
      }
    } with lbl1: {
    }
  }
}


