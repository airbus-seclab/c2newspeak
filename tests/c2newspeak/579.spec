Warning: 579.c:31#0: goto statement accepted
Newspeak output
---------------
579.c
void main(void) {
  (579.c:27#6)^uint32 goto.lbl;
  (579.c:27#6)^0- =(uint32) 0;
  (579.c:27#6)^{
    int32 i;
    (579.c:28#2)^do {
      (579.c:28#2)^while (1) {
        (579.c:28#2)^do {
          (579.c:28#2)^while (1) {
            (579.c:28#2)^choose {
             -->
              (579.c:28#2)^choose {
               -->
                (579.c:28#2)^guard(! (1-_uint32 ==_uint32 0));
               -->
                (579.c:28#2)^guard((1-_uint32 ==_uint32 0));
                (579.c:28#2)^guard(! (0-_int32 ==_int32 0));
              }
             -->
              (579.c:28#2)^guard((1-_uint32 ==_uint32 0));
              (579.c:28#2)^guard((0-_int32 ==_int32 0));
              (579.c:28#2)^goto lbl3;
            }
            (579.c:29#2)^1- =(uint32) 0;
            (579.c:29#7)^0- =(int32) 0;
          }
        } with lbl3: {
        }
        (579.c:28#2)^1- =(uint32) 1;
        (579.c:28#2)^choose {
         -->
          (579.c:28#2)^guard(1-_int32);
         -->
          (579.c:28#2)^guard(! 1-_int32);
          (579.c:28#2)^goto lbl1;
        }
      }
    } with lbl1: {
    }
  }
}


