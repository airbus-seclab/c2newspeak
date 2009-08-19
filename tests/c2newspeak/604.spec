Warning: 604.c:32#0: goto statement accepted
Newspeak output
---------------
604.c
void main(void) {
  (604.c:27#6)^uint32 goto.lbl;
  (604.c:27#6)^0- =(uint32) 0;
  (604.c:27#6)^{
    int32 i;
    (604.c:28#2)^do {
      (604.c:28#2)^while (1) {
        (604.c:28#2)^do {
          (604.c:28#2)^while (1) {
            (604.c:28#2)^choose {
             -->
              (604.c:28#2)^choose {
               -->
                (604.c:28#2)^guard(! (1-_uint32 ==_uint32 0));
               -->
                (604.c:28#2)^guard((1-_uint32 ==_uint32 0));
                (604.c:28#2)^guard(! (0-_int32 ==_int32 0));
              }
             -->
              (604.c:28#2)^guard((1-_uint32 ==_uint32 0));
              (604.c:28#2)^guard((0-_int32 ==_int32 0));
              (604.c:28#2)^goto lbl3;
            }
            (604.c:28#12)^choose {
             -->
              (604.c:28#12)^guard(! 1-_int32);
              (604.c:29#4)^0- =(int32) 0;
             -->
              (604.c:28#12)^guard(1-_int32);
            }
            (604.c:30#2)^1- =(uint32) 0;
            (604.c:30#7)^0- =(int32) 1;
          }
        } with lbl3: {
        }
        (604.c:28#2)^1- =(uint32) 1;
        (604.c:28#2)^choose {
         -->
          (604.c:28#2)^guard(1-_int32);
         -->
          (604.c:28#2)^guard(! 1-_int32);
          (604.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


