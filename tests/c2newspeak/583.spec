Warning: 583.c:30#1087: goto statement accepted
Newspeak output
---------------
583.c
void main(void) {
  (583.c:27#6)^uint32 goto.lbl;
  (583.c:27#6)^0- =(uint32) 0;
  (583.c:27#6)^{
    int32 i;
    (583.c:29#2)^0- =(int32) 1;
    (583.c:28#1)^do {
      (583.c:30#2)^1- =(uint32) 1;
      (583.c:30#2)^choose {
       -->
        (583.c:30#2)^guard(1-_uint32);
        (583.c:30#2)^goto lbl2;
       -->
        (583.c:30#2)^guard(! 1-_uint32);
      }
      (583.c:29#2)^while (1) {
        (583.c:29#2)^choose {
         -->
          (583.c:29#2)^guard(! (0-_int32 ==_int32 0));
         -->
          (583.c:29#2)^guard((0-_int32 ==_int32 0));
          (583.c:29#2)^goto lbl2;
        }
        (583.c:30#2)^1- =(uint32) 1;
        (583.c:30#2)^choose {
         -->
          (583.c:30#2)^guard(1-_uint32);
          (583.c:30#2)^goto lbl2;
         -->
          (583.c:30#2)^guard(! 1-_uint32);
        }
      }
    } with lbl2: {
    }
    (583.c:28#1)^do {
      (583.c:28#1)^while (1) {
        (583.c:28#1)^choose {
         -->
          (583.c:28#1)^guard(1-_uint32);
         -->
          (583.c:28#1)^guard(! 1-_uint32);
          (583.c:28#1)^goto lbl1;
        }
        (583.c:29#2)^0- =(int32) 1;
        (583.c:28#1)^do {
          (583.c:30#2)^1- =(uint32) 1;
          (583.c:30#2)^choose {
           -->
            (583.c:30#2)^guard(1-_uint32);
            (583.c:30#2)^goto lbl6;
           -->
            (583.c:30#2)^guard(! 1-_uint32);
          }
          (583.c:29#2)^while (1) {
            (583.c:29#2)^choose {
             -->
              (583.c:29#2)^guard(! (0-_int32 ==_int32 0));
             -->
              (583.c:29#2)^guard((0-_int32 ==_int32 0));
              (583.c:29#2)^goto lbl6;
            }
            (583.c:30#2)^1- =(uint32) 1;
            (583.c:30#2)^choose {
             -->
              (583.c:30#2)^guard(1-_uint32);
              (583.c:30#2)^goto lbl6;
             -->
              (583.c:30#2)^guard(! 1-_uint32);
            }
          }
        } with lbl6: {
        }
      }
    } with lbl1: {
    }
  }
}


