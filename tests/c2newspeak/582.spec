Warning: 582.c:31#0: goto statement accepted
Newspeak output
---------------
582.c
void main(void) {
  (582.c:27#6)^uint32 goto.lbl;
  (582.c:27#6)^0- =(uint32) 0;
  (582.c:27#6)^{
    int32 i;
    (582.c:28#2)^do {
      (582.c:28#2)^while (1) {
        (582.c:28#2)^do {
          (582.c:28#2)^while (1) {
            (582.c:29#2)^1- =(uint32) 0;
            (582.c:29#7)^0- =(int32) 0;
            (582.c:28#2)^choose {
             -->
              (582.c:28#2)^guard(! (0-_int32 ==_int32 0));
             -->
              (582.c:28#2)^guard((0-_int32 ==_int32 0));
              (582.c:28#2)^goto lbl3;
            }
          }
        } with lbl3: {
        }
        (582.c:28#2)^1- =(uint32) 1;
        (582.c:28#2)^choose {
         -->
          (582.c:28#2)^guard(1-_int32);
         -->
          (582.c:28#2)^guard(! 1-_int32);
          (582.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


