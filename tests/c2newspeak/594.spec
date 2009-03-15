Warning: 594.c:32#1110: goto statement accepted
Newspeak output
---------------
594.c
void main(void) {
  (594.c:27#6)^uint32 goto.lbl;
  (594.c:27#6)^0- =(uint32) 0;
  (594.c:27#6)^{
    int32 i;
    (594.c:31#2)^do {
      (594.c:31#2)^while (1) {
        (594.c:28#2)^1- =(uint32) 1-_uint32;
        (594.c:28#2)^do {
          (594.c:28#2)^while (1) {
            (594.c:28#2)^choose {
             -->
              (594.c:28#2)^guard(1-_uint32);
             -->
              (594.c:28#2)^guard(! 1-_uint32);
              (594.c:28#2)^choose {
               -->
                (594.c:28#2)^guard(! (0-_int32 ==_int32 0));
               -->
                (594.c:28#2)^guard((0-_int32 ==_int32 0));
                (594.c:28#2)^goto lbl3;
              }
            }
            (594.c:29#2)^1- =(uint32) 0;
            (594.c:29#7)^0- =(int32) 0;
          }
        } with lbl3: {
        }
        (594.c:31#2)^choose {
         -->
          (594.c:31#2)^guard(! (0-_int32 ==_int32 0));
          (594.c:31#2)^1- =(uint32) 1;
         -->
          (594.c:31#2)^guard((0-_int32 ==_int32 0));
        }
        (594.c:31#2)^1- =(uint32) 1-_uint32;
        (594.c:31#2)^choose {
         -->
          (594.c:31#2)^guard(1-_uint32);
         -->
          (594.c:31#2)^guard(! 1-_uint32);
          (594.c:31#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


