Warning: 586.c:34#1133: goto statement accepted
Newspeak output
---------------
586.c
void main(void) {
  (586.c:27#6)^uint32 goto.lbl;
  (586.c:27#6)^0- =(uint32) 0;
  (586.c:27#6)^{
    int32 i;
    (586.c:28#2)^1- =(uint32) 1-_uint32;
    (586.c:28#2)^{
      uint32 switch.586.c:28#2;
      (586.c:28#2)^choose {
       -->
        (586.c:28#2)^guard(! 2-_uint32);
        (586.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
       -->
        (586.c:28#2)^guard(2-_uint32);
        (586.c:28#2)^0- =(uint32) 1;
      }
      (586.c:28#2)^do {
        (586.c:29#2)^do {
          (586.c:28#2)^choose {
           -->
            (586.c:28#2)^guard((0-_uint32 ==_int32 1));
            (586.c:29#2)^goto lbl5;
           -->
            (586.c:28#2)^guard(! (0-_uint32 ==_int32 1));
            (586.c:28#2)^goto lbl4;
          }
        } with lbl5: {
        }
        (586.c:30#7)^1- =(int32) 0;
      } with lbl4: {
      }
      (586.c:32#4)^1- =(int32) 2;
      (586.c:34#2)^2- =(uint32) 1;
    }
    (586.c:34#2)^do {
      (586.c:34#2)^while (1) {
        (586.c:34#2)^choose {
         -->
          (586.c:34#2)^guard(1-_uint32);
         -->
          (586.c:34#2)^guard(! 1-_uint32);
          (586.c:34#2)^goto lbl1;
        }
        (586.c:28#2)^1- =(uint32) 1-_uint32;
        (586.c:28#2)^{
          uint32 switch.586.c:28#2;
          (586.c:28#2)^choose {
           -->
            (586.c:28#2)^guard(! 2-_uint32);
            (586.c:28#2)^0- =(uint32) coerce[0,4294967295] 1-_int32;
           -->
            (586.c:28#2)^guard(2-_uint32);
            (586.c:28#2)^0- =(uint32) 1;
          }
          (586.c:28#2)^do {
            (586.c:29#2)^do {
              (586.c:28#2)^choose {
               -->
                (586.c:28#2)^guard((0-_uint32 ==_int32 1));
                (586.c:29#2)^goto lbl10;
               -->
                (586.c:28#2)^guard(! (0-_uint32 ==_int32 1));
                (586.c:28#2)^goto lbl9;
              }
            } with lbl10: {
            }
            (586.c:30#7)^1- =(int32) 0;
          } with lbl9: {
          }
          (586.c:32#4)^1- =(int32) 2;
          (586.c:34#2)^2- =(uint32) 1;
        }
      }
    } with lbl1: {
    }
  }
}


