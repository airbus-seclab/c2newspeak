Warning: 601.c:30#0: goto statement accepted
Newspeak output
---------------
601.c
void main(void) {
  (601.c:27#6)^uint32 goto.lbl;
  (601.c:27#6)^0- =(uint32) 0;
  (601.c:27#6)^{
    int32 i;
    (601.c:28#2)^uint32 switch.601.c:28#2.0;
    (601.c:28#2)^do {
      (601.c:28#2)^while (1) {
        (601.c:28#2)^choose {
         -->
          (601.c:28#2)^guard(! 2-_int32);
          (601.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
         -->
          (601.c:28#2)^guard(2-_int32);
          (601.c:28#2)^0- =(uint32) 1;
        }
        (601.c:28#2)^do {
          (601.c:28#2)^do {
            (601.c:29#2)^do {
              (601.c:28#2)^choose {
               -->
                (601.c:28#2)^guard((0-_uint32 ==_int32 0));
                (601.c:29#2)^goto lbl5;
               -->
                (601.c:28#2)^guard(! (0-_uint32 ==_int32 0));
                (601.c:28#2)^goto lbl4;
              }
            } with lbl5: {
            }
            (601.c:30#4)^2- =(uint32) 1;
            (601.c:30#4)^choose {
             -->
              (601.c:30#4)^guard(2-_int32);
              (601.c:30#4)^goto lbl3;
             -->
              (601.c:30#4)^guard(! 2-_int32);
            }
          } with lbl4: {
          }
          (601.c:32#4)^while (1) {
            (601.c:32#4)^choose {
             -->
              (601.c:32#4)^guard(! (2-_uint32 ==_uint32 0));
             -->
              (601.c:32#4)^guard((2-_uint32 ==_uint32 0));
            }
            (601.c:33#4)^2- =(uint32) 0;
          }
        } with lbl3: {
        }
        (601.c:28#2)^choose {
         -->
          (601.c:28#2)^guard(2-_int32);
         -->
          (601.c:28#2)^guard(! 2-_int32);
          (601.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


