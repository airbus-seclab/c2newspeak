Warning: 635.c:39#1174: goto statement accepted
Newspeak output
---------------
635.c
void main(void) {
  (635.c:27#2)^uint32 goto.lbl;
  (635.c:27#2)^0- =(uint32) 0;
  (635.c:37#2)^do {
    (635.c:37#2)^while (1) {
      (635.c:27#2)^while (1) {
        (635.c:27#2)^choose {
         -->
          (635.c:27#2)^guard(! (0-_uint32 ==_uint32 0));
         -->
          (635.c:27#2)^guard((0-_uint32 ==_uint32 0));
        }
        (635.c:29#10)^{
          int32 m;
          (635.c:33#10)^choose {
           -->
            (635.c:33#10)^guard((1-_uint32 ==_uint32 0));
            (635.c:31#1)^0- =(int32) 0;
           -->
            (635.c:33#10)^guard(! (1-_uint32 ==_uint32 0));
          }
        }
      }
      (635.c:37#2)^do {
        (635.c:37#2)^while (1) {
          (635.c:39#4)^0- =(uint32) 1;
          (635.c:39#4)^choose {
           -->
            (635.c:39#4)^guard(0-_int32);
            (635.c:39#4)^goto lbl7;
           -->
            (635.c:39#4)^guard(! 0-_int32);
          }
        }
      } with lbl7: {
      }
      (635.c:37#2)^choose {
       -->
        (635.c:37#2)^guard(0-_int32);
       -->
        (635.c:37#2)^guard(! 0-_int32);
        (635.c:37#2)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}


