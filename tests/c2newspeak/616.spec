Warning: 616.c:32#1131: goto statement accepted
Newspeak output
---------------
616.c
void main(void) {
  (616.c:27#4)^uint32 goto.lbl;
  (616.c:27#4)^0- =(uint32) 0;
  (616.c:28#6)^{
    uint32 break.616.c:31#13.0;
    (616.c:27#4)^do {
      (616.c:27#4)^while (1) {
        (616.c:28#6)^0- =(uint32) 0;
        (616.c:31#6)^do {
          (616.c:28#6)^1- =(uint32) 1-_uint32;
          (616.c:28#6)^while (1) {
            (616.c:28#6)^choose {
             -->
              (616.c:28#6)^guard(1-_uint32);
             -->
              (616.c:28#6)^guard(! 1-_uint32);
            }
            (616.c:29#6)^1- =(uint32) 0;
          }
          (616.c:31#13)^0- =(uint32) 1;
          (616.c:31#13)^goto lbl4;
          (616.c:31#6)^1- =(uint32) 1;
          (616.c:31#6)^while (1) {
            (616.c:31#6)^choose {
             -->
              (616.c:31#6)^guard(1-_uint32);
             -->
              (616.c:31#6)^guard(! 1-_uint32);
              (616.c:31#6)^goto lbl4;
            }
            (616.c:28#6)^1- =(uint32) 1-_uint32;
            (616.c:28#6)^while (1) {
              (616.c:28#6)^choose {
               -->
                (616.c:28#6)^guard(1-_uint32);
               -->
                (616.c:28#6)^guard(! 1-_uint32);
              }
              (616.c:29#6)^1- =(uint32) 0;
            }
            (616.c:31#13)^0- =(uint32) 1;
            (616.c:31#13)^goto lbl4;
            (616.c:31#6)^1- =(uint32) 1;
          }
        } with lbl4: {
        }
        (616.c:28#6)^choose {
         -->
          (616.c:28#6)^guard(0-_uint32);
          (616.c:28#6)^0- =(uint32) 0;
          (616.c:28#6)^goto lbl1;
         -->
          (616.c:28#6)^guard(! 0-_uint32);
        }
      }
    } with lbl1: {
    }
  }
}


